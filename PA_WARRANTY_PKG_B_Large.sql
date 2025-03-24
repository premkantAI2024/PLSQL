create or replace PACKAGE BODY GETS_PA_WARRANTY_PKG AS
  -- Purpose: This package is used to create Warranty Reserve, Cost Calculation and Report for business users
  --
  --  Date          Name                  Version           Remarks
  -----------------------------------------------------------------------------------------------------------------------
  -- 06-Feb-2025    Tanmoy C.             1.0               Initial version
  -----------------------------------------------------------------------------------------------------------------------
  -----------------------------------------------------------------------------------------------------------------------
  -- Main procedure to be called by custom Warranty concurrent program
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE main(errbuf               OUT VARCHAR2,
                 retcode              OUT NUMBER,
                 p_project_num        IN pa_projects_all.segment1%TYPE,
                 p_project_type       IN pa_projects_all.project_type%TYPE,
                 p_ou_name            IN gets_inv_op_unit_info.short_name%TYPE,
                 p_task_number        IN pa_tasks.task_number%TYPE DEFAULT NULL,
                 p_debug_level        IN NUMBER,
                 p_transaction_source IN VARCHAR2,
                 p_exp_type           IN VARCHAR2) AS
  
  BEGIN
    errbuf        := 'Sucessfully Completed';
    retcode       := 0;
    g_debug_level := p_debug_level;
    g_task_number := nvl(p_task_number, '14001-WCP');
    g_trx_source  := 'PSC LEGACY AR CONV';
    g_user_id     := FND_GLOBAL.USER_ID;
    g_resp_id     := FND_GLOBAL.RESP_ID;
    g_resp_app_id := FND_GLOBAL.RESP_APPL_ID;
  
    dbms_output.put_line('MAIN start=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'MAIN start=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    IF p_transaction_source = 'Warranty Provision' THEN
      -- Call PROCESS_PA 
      BEGIN
        process_pa(errbuf,
                   retcode,
                   p_project_num,
                   p_project_type,
                   p_ou_name,
                   p_task_number,
                   p_transaction_source,
                   p_exp_type);
      EXCEPTION
        WHEN OTHERS THEN
          errbuf  := SUBSTR('Error calling PROCESS_PA procedure, error=>' ||
                            SQLERRM,
                            1,
                            255);
          retcode := 2;
          ROLLBACK;
          RETURN;
      END;
    ELSIF p_transaction_source = 'Warranty Cost' THEN
      BEGIN
        warranty_cost(errbuf,
                      retcode,
                      p_project_num,
                      p_project_type,
                      p_ou_name,
                      p_task_number,
                      p_transaction_source,
                      p_exp_type);
      EXCEPTION
        WHEN OTHERS THEN
          errbuf  := SUBSTR('Error calling WARRANTY_COST procedure, error=>' ||
                            SQLERRM,
                            1,
                            255);
          retcode := 2;
          ROLLBACK;
          RETURN;
      END;
    END IF;
    -- Call ELIGIBLE_PA_BATCH
    BEGIN
      eligible_pa_batch(errbuf, retcode);
    EXCEPTION
      WHEN OTHERS THEN
        errbuf  := SUBSTR('Error calling ELIGIBLE_PA_BATCH procedure, error=>' ||
                          SQLERRM,
                          1,
                          255);
        retcode := 2;
        ROLLBACK;
        RETURN;
    END;
    dbms_output.put_line('MAIN completed=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'MAIN completed=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := SUBSTR('Unexpected Error in MAIN procedure, error=>' ||
                        SQLERRM,
                        1,
                        255);
      retcode := 2;
      ROLLBACK;
      RETURN;
  END main;
  -----------------------------------------------------------------------------------------------------------------------
  -- Procedure to process and validate the invoices and enter into custom interface table
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE process_pa(errbuf               OUT VARCHAR2,
                       retcode              OUT NUMBER,
                       p_project_num        IN pa_projects_all.segment1%TYPE,
                       p_project_type       IN pa_projects_all.project_type%TYPE,
                       p_ou_name            IN gets_inv_op_unit_info.short_name%TYPE,
                       p_task_number        IN pa_tasks.task_number%TYPE DEFAULT NULL,
                       p_transaction_source IN VARCHAR2,
                       p_exp_type           IN VARCHAR2) AS
  
    CURSOR c1_cur IS
      SELECT pa.segment1,
             pa.project_id,
             ppt.project_type,
             pa.org_id,
             hou.short_name pa_org
        FROM pa_projects_all       PA,
             pa_project_types_all  ppt,
             gets_inv_op_unit_info hou
       WHERE pa.project_type = ppt.project_type
         AND pa.org_id = ppt.org_id
         and pa.org_id = hou.org_id
         AND pa.template_flag = 'N'
         AND pa.project_status_code = 'APPROVED'
         AND pa.project_type = nvl(p_project_type, pa.project_type)
         AND pa.project_type IN
             (SELECT meaning
                FROM fnd_lookup_values_vl
               WHERE lookup_type = 'GETS_PA_WARRANTY_PRJ_TYPES'
                 AND REGEXP_SUBSTR(lookup_code, '[^_]+', 1, 1) =
                     hou.short_name
                 AND enabled_flag = 'Y'
                 AND trunc(SYSDATE) BETWEEN
                     nvl(start_date_active, trunc(SYSDATE)) AND
                     nvl(end_date_active, trunc(SYSDATE)))
         AND pa.segment1 = nvl(p_project_num, pa.segment1)
         AND hou.short_name = nvl(p_ou_name, hou.short_name)
       ORDER BY pa.project_id;
  
    v_pa_custom_intf_rec         gets_pa_transaction_intf_all%ROWTYPE;
    v_orig_transaction_reference gets_pa_transaction_intf_all.orig_transaction_reference%TYPE := NULL;
    v_cost_budget                NUMBER := 0;
    v_revenue_budget             NUMBER := 0;
    v_warranty_reserve_actual    NUMBER := 0;
    v_revenue_total_amount       NUMBER := 0;
    v_ar_revenue                 NUMBER := 0;
    v_pa_revenue                 NUMBER := 0;
    v_accumulated_warranty       NUMBER := 0;
    v_exp_date                   DATE := SYSDATE;
    v_error_flag                 VARCHAR2(1);
    v_task_id                    pa_tasks.task_id%TYPE;
    v_task_num                   pa_tasks.task_number%TYPE;
    v_task_name                  pa_tasks.task_name%TYPE;
    v_error_message              VARCHAR2(2000);
  -----------Added 6th March----------------------
    v_subproject_ar_rev_amt NUMBER := 0;
    v_subproject_pa_rev_amt NUMBER := 0;
    v_sub_rev_amt NUMBER :=0;
    TYPE v_project_id_typ IS TABLE OF pa_projects_all.project_id%TYPE INDEX BY PLS_INTEGER;
    TYPE v_org_id_typ IS TABLE OF pa_projects_all.org_id%TYPE INDEX BY PLS_INTEGER;
    TYPE v_segment1_typ IS TABLE OF pa_projects_all.segment1%TYPE INDEX BY PLS_INTEGER;
    v_project_id_list v_project_id_typ;
    v_org_id_list v_org_id_typ;
    v_segment1_list v_segment1_typ;
  BEGIN
    errbuf       := 'Sucessfully Completed';
    retcode      := 0;
    v_error_flag := 'N';
    dbms_output.put_line('PROCESS_PA start=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'PROCESS_PA start=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
  
    FOR c1_rec IN c1_cur LOOP
      v_cost_budget             := 0;
      v_revenue_budget          := 0;
      v_warranty_reserve_actual := 0;
      v_revenue_total_amount    := 0;
      v_ar_revenue              := 0;
      v_pa_revenue              := 0;
      v_accumulated_warranty    := 0;
    
      BEGIN
        SELECT pt.task_id, pt.task_number, pt.task_name
          INTO v_task_id, v_task_num, v_task_name
          FROM pa_tasks pt
         WHERE pt.project_id = c1_rec.project_id
           AND pt.task_number = nvl(p_task_number, g_task_number);
      EXCEPTION
        WHEN OTHERS THEN
          v_task_id    := NULL;
          v_task_num   := NULL;
          v_task_name  := NULL;
          v_error_flag := 'Y';
      END;
      IF v_error_flag <> 'Y' THEN
        BEGIN
          v_revenue_budget := get_budget_details(c1_rec.project_id,
                                                 v_task_id,
                                                 'AR');
          v_cost_budget    := get_budget_details(c1_rec.project_id,
                                                 v_task_id,
                                                 'AC');
          IF v_cost_budget = 0 THEN
            v_error_flag    := 'Y';
            v_error_message := 'No Cost budget for Project: ' ||
                               c1_rec.segment1 || ' and Task: ' ||
                               g_task_number;
          ELSIF v_revenue_budget = 0 THEN
            v_error_flag    := 'Y';
            v_error_message := 'No Revenue budget for Project: ' ||
                               c1_rec.segment1;
          ELSE
            v_error_flag := 'N';
          END IF;
          IF v_error_flag <> 'Y' THEN
          -----------Added 6th March----------------------
          BEGIN
                SELECT ppa.project_id,ppa.org_id,ppa.segment1
      BULK COLLECT
      INTO v_project_id_list,v_org_id_list,v_segment1_list
      FROM pa_tasks pt, pa_projects_all ppa
      WHERE 1 = 1
      AND pt.task_number = ppa.segment1
      AND pt.project_id = c1_rec.project_id
      AND pt.task_name = 'Sub Project';
      IF (v_project_id_list.count > 0) THEN
      v_subproject_ar_rev_amt := 0;
      v_subproject_pa_rev_amt := 0;
      v_sub_rev_amt := 0;
         FOR i IN v_project_id_list.first .. v_project_id_list.last
         LOOP
            v_subproject_ar_rev_amt := v_subproject_ar_rev_amt +
                                       get_ar_revenue(v_org_id_list(i),v_segment1_list(i));
         END LOOP;
	         FOR j IN v_project_id_list.first .. v_project_id_list.last
         LOOP
            v_subproject_pa_rev_amt := v_subproject_pa_rev_amt +
                                       get_revenue_totals(v_project_id_list(j));
         END LOOP;
      END IF;
      v_sub_rev_amt := v_subproject_pa_rev_amt + v_subproject_ar_rev_amt;
fnd_file.put_line(fnd_file.LOG,
                                'Sub Project Revenue amount for Project: ' ||
                                c1_rec.segment1 || ' is: ' ||
                                v_sub_rev_amt);
        EXCEPTION
        WHEN OTHERS THEN
          v_sub_rev_amt    := 0;
          END;
               -----------END OF CHANGE 6th March----------------------   
            v_pa_revenue              := get_revenue_totals(c1_rec.project_id);
            v_ar_revenue              := get_ar_revenue(c1_rec.org_id,
                                                        c1_rec.segment1);
            v_revenue_total_amount    := v_pa_revenue + v_ar_revenue + v_sub_rev_amt;
            v_accumulated_warranty    := get_accm_warranty(c1_rec.org_id,
                                                           c1_rec.project_id,
                                                           v_task_id,
                                                           p_exp_type,
                                                           p_transaction_source);
            v_warranty_reserve_actual := round((((v_cost_budget /
                                               v_revenue_budget) *
                                               v_revenue_total_amount) -
                                               v_accumulated_warranty),
                                               2);
            IF v_warranty_reserve_actual <> 0 THEN
              BEGIN
                v_pa_custom_intf_rec.transaction_source    := p_transaction_source;
                v_pa_custom_intf_rec.batch_name            := p_transaction_source || '_' ||
                                                              to_char(SYSDATE,
                                                                      'YYYY-MM-DD HH:MM:SS');
                v_pa_custom_intf_rec.organization_name     := get_project_org(c1_rec.project_id);
                v_pa_custom_intf_rec.expenditure_item_date := v_exp_date;
                IF trim(to_char(v_exp_date, 'DAY')) = 'SUNDAY' THEN
                  v_pa_custom_intf_rec.expenditure_ending_date := v_exp_date;
                ELSE
                  v_pa_custom_intf_rec.expenditure_ending_date := next_day(v_exp_date,
                                                                           'SUNDAY');
                END IF;
                print_debug_message('1');
                v_pa_custom_intf_rec.project_number              := c1_rec.segment1;
                v_pa_custom_intf_rec.expenditure_type            := p_exp_type;
                v_pa_custom_intf_rec.quantity                    := 1;
                v_pa_custom_intf_rec.raw_cost                    := v_warranty_reserve_actual;
                v_pa_custom_intf_rec.burdened_cost               := v_warranty_reserve_actual;
                v_pa_custom_intf_rec.transaction_status_code     := 'P';
                v_pa_custom_intf_rec.project_id                  := c1_rec.project_id;
                v_pa_custom_intf_rec.task_number                 := v_task_num;
                v_pa_custom_intf_rec.task_id                     := v_task_id;
                v_pa_custom_intf_rec.system_linkage              := NULL;
                v_pa_custom_intf_rec.unmatched_negative_txn_flag := 'Y';
                v_pa_custom_intf_rec.org_id                      := c1_rec.org_id;
                v_pa_custom_intf_rec.created_by                  := fnd_global.user_id;
                v_pa_custom_intf_rec.creation_date               := SYSDATE;
                v_pa_custom_intf_rec.last_updated_by             := fnd_global.user_id;
                v_pa_custom_intf_rec.last_update_date            := SYSDATE;
                print_debug_message('1.1');
                v_pa_custom_intf_rec.expenditure_comment         := '[((' ||
                                                                    v_cost_budget || '/' ||
                                                                    v_revenue_budget || ')*' ||
                                                                    v_revenue_total_amount || ')-' ||
                                                                    v_accumulated_warranty ||
                                                                    '] = ' ||
                                                                    v_warranty_reserve_actual;
                v_pa_custom_intf_rec.interface_id                := NULL;
                v_pa_custom_intf_rec.user_transaction_source     := NULL;
                v_pa_custom_intf_rec.employee_number             := NULL;
                v_pa_custom_intf_rec.non_labor_resource          := NULL;
                v_pa_custom_intf_rec.non_labor_resource_org_name := NULL;
                v_pa_custom_intf_rec.transaction_rejection_code  := NULL;
                v_pa_custom_intf_rec.expenditure_id              := NULL;
                v_pa_custom_intf_rec.attribute_category          := NULL;
                v_pa_custom_intf_rec.attribute1                  := c1_rec.segment1;
                v_pa_custom_intf_rec.attribute2                  := c1_rec.project_type;
                v_pa_custom_intf_rec.attribute3                  := v_pa_revenue || ';' ||
                                                                    v_ar_revenue;
                v_pa_custom_intf_rec.attribute4                  := v_revenue_total_amount;
                v_pa_custom_intf_rec.attribute5                  := v_revenue_budget;
                v_pa_custom_intf_rec.attribute6                  := v_cost_budget;
                v_pa_custom_intf_rec.attribute7                  := v_accumulated_warranty;
                v_pa_custom_intf_rec.attribute8                  := to_char(SYSDATE,
                                                                            'MON-YY');
                print_debug_message('1.2');
                v_pa_custom_intf_rec.attribute9 := v_pa_custom_intf_rec.batch_name;
                print_debug_message('1.3');
                v_pa_custom_intf_rec.attribute10 := v_warranty_reserve_actual;
                print_debug_message('1.4');
                v_pa_custom_intf_rec.raw_cost_rate                 := NULL;
                v_pa_custom_intf_rec.expenditure_item_id           := NULL;
                v_pa_custom_intf_rec.dr_code_combination_id        := NULL;
                v_pa_custom_intf_rec.cr_code_combination_id        := NULL;
                v_pa_custom_intf_rec.cdl_system_reference1         := NULL;
                v_pa_custom_intf_rec.cdl_system_reference2         := NULL;
                v_pa_custom_intf_rec.cdl_system_reference3         := NULL;
                v_pa_custom_intf_rec.gl_date                       := NULL;
                v_pa_custom_intf_rec.burdened_cost_rate            := NULL;
                v_pa_custom_intf_rec.receipt_currency_amount       := NULL;
                v_pa_custom_intf_rec.receipt_currency_code         := NULL;
                v_pa_custom_intf_rec.receipt_exchange_rate         := NULL;
                v_pa_custom_intf_rec.denom_currency_code           := NULL;
                v_pa_custom_intf_rec.denom_raw_cost                := v_warranty_reserve_actual;
                v_pa_custom_intf_rec.denom_burdened_cost           := NULL;
                v_pa_custom_intf_rec.acct_rate_date                := NULL;
                v_pa_custom_intf_rec.acct_rate_type                := NULL;
                v_pa_custom_intf_rec.acct_exchange_rate            := NULL;
                v_pa_custom_intf_rec.acct_raw_cost                 := NULL;
                v_pa_custom_intf_rec.acct_burdened_cost            := NULL;
                v_pa_custom_intf_rec.acct_exchange_rounding_limit  := NULL;
                v_pa_custom_intf_rec.project_currency_code         := NULL;
                v_pa_custom_intf_rec.project_rate_date             := NULL;
                v_pa_custom_intf_rec.project_rate_type             := NULL;
                v_pa_custom_intf_rec.project_exchange_rate         := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference1       := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference2       := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference3       := NULL;
                v_pa_custom_intf_rec.orig_user_exp_txn_reference   := NULL;
                v_pa_custom_intf_rec.override_to_organization_name := NULL;
                v_pa_custom_intf_rec.reversed_orig_txn_reference   := NULL;
                v_pa_custom_intf_rec.billable_flag                 := NULL;
                v_pa_custom_intf_rec.person_business_group_name    := NULL;
                v_pa_custom_intf_rec.projfunc_currency_code        := NULL;
                v_pa_custom_intf_rec.projfunc_cost_rate_type       := NULL;
                v_pa_custom_intf_rec.projfunc_cost_rate_date       := NULL;
                v_pa_custom_intf_rec.projfunc_cost_exchange_rate   := NULL;
                v_pa_custom_intf_rec.project_raw_cost              := NULL;
                v_pa_custom_intf_rec.project_burdened_cost         := NULL;
                v_pa_custom_intf_rec.assignment_name               := NULL;
                v_pa_custom_intf_rec.work_type_name                := NULL;
                v_pa_custom_intf_rec.cdl_system_reference4         := NULL;
                v_pa_custom_intf_rec.accrual_flag                  := NULL;
                v_pa_custom_intf_rec.person_id                     := NULL;
                v_pa_custom_intf_rec.organization_id               := NULL;
                v_pa_custom_intf_rec.non_labor_resource_org_id     := NULL;
                v_pa_custom_intf_rec.override_to_organization_id   := NULL;
                v_pa_custom_intf_rec.assignment_id                 := NULL;
                v_pa_custom_intf_rec.work_type_id                  := NULL;
                v_pa_custom_intf_rec.person_business_group_id      := NULL;
                v_pa_custom_intf_rec.inventory_item_id             := NULL;
                v_pa_custom_intf_rec.wip_resource_id               := NULL;
                v_pa_custom_intf_rec.unit_of_measure               := NULL;
                v_pa_custom_intf_rec.po_number                     := NULL;
                v_pa_custom_intf_rec.po_header_id                  := NULL;
                v_pa_custom_intf_rec.po_line_num                   := NULL;
                v_pa_custom_intf_rec.po_line_id                    := NULL;
                v_pa_custom_intf_rec.person_type                   := NULL;
                v_pa_custom_intf_rec.po_price_type                 := NULL;
                v_pa_custom_intf_rec.vendor_id                     := NULL;
                v_pa_custom_intf_rec.vendor_number                 := NULL;
                v_pa_custom_intf_rec.status                        := 'ELIGIBLE';
                v_pa_custom_intf_rec.status_message                := NULL;
                print_debug_message('1.5');
                v_orig_transaction_reference := get_orig_transaction_reference(c1_rec.org_id,
                                                                               c1_rec.project_id,
                                                                               v_task_id,
                                                                               p_exp_type,
                                                                               p_transaction_source);
                print_debug_message('2');
                IF v_orig_transaction_reference IS NULL THEN
                  print_debug_message('3');
                  v_pa_custom_intf_rec.orig_transaction_reference := gets_pa_orig_trans_ref_s.NEXTVAL;
                  BEGIN
                    gets_pa_interface_ins(errbuf,
                                          retcode,
                                          v_pa_custom_intf_rec);
                  EXCEPTION
                    WHEN OTHERS THEN
                      errbuf  := SUBSTR('Error calling gets_pa_interface_ins, error=>' ||
                                        SQLERRM,
                                        1,
                                        255);
                      retcode := 2;
                      ROLLBACK;
                      RETURN;
                  END;
                ELSE
                  print_debug_message('4');
                  v_pa_custom_intf_rec.orig_transaction_reference := v_orig_transaction_reference;
                  BEGIN
                    gets_pa_interface_upd(errbuf,
                                          retcode,
                                          v_pa_custom_intf_rec);
                  EXCEPTION
                    WHEN OTHERS THEN
                      errbuf  := SUBSTR('Error calling gets_pa_interface_upd, error=>' ||
                                        SQLERRM,
                                        1,
                                        255);
                      retcode := 2;
                      ROLLBACK;
                      RETURN;
                  END;
                END IF;
                print_debug_message('5');
              EXCEPTION
                WHEN OTHERS THEN
                  errbuf  := SUBSTR('Error assigning data to v_pa_custom_intf_rec, error=>' ||
                                    SQLERRM,
                                    1,
                                    255);
                  retcode := 2;
                  ROLLBACK;
                  RETURN;
              END;
            ELSE
              fnd_file.put_line(fnd_file.LOG,
                                'No Warranty Reserve actual amount for Project: ' ||
                                c1_rec.segment1 || ' and Task: ' ||
                                v_task_num);
              fnd_file.put_line(fnd_file.LOG,
                                'Warranty Reserve Actual => [((Forecast Warranty Reserve/Forecast Sales)*Actual Sales)-Accumulated Warranty Reserve]' ||
                                ' = ' || '[((' || v_cost_budget || '/' ||
                                v_revenue_budget || ')*' ||
                                v_revenue_total_amount || ')-' ||
                                v_accumulated_warranty || '] = ' ||
                                v_warranty_reserve_actual);
            END IF;
          ELSE
            fnd_file.put_line(fnd_file.LOG, v_error_message);
          END IF;
        EXCEPTION
          WHEN OTHERS THEN
            fnd_file.put_line(fnd_file.LOG, 'Error: ' || SQLERRM);
        END;
      ELSE
        fnd_file.put_line(fnd_file.LOG,
                          g_task_number ||
                          ' task not present for Project: ' ||
                          c1_rec.segment1);
      END IF;
    END LOOP;
    COMMIT;
    dbms_output.put_line('PROCESS_PA completed=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'PROCESS_PA completed=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := SUBSTR('Unexpected Error in PROCESS_PA procedure, error=>' ||
                        SQLERRM,
                        1,
                        255);
      retcode := 2;
      ROLLBACK;
      RETURN;
  END process_pa;
  -----------------------------------------------------------------------------------------------------------------------
  -- Procedure to group eligible batches in std PA interface and call PRC: Transaction Import
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE eligible_pa_batch(errbuf OUT VARCHAR2, retcode OUT NUMBER) AS
  
    CURSOR c1_cur IS
      SELECT DISTINCT transaction_source, batch_name
        FROM gets_pa_transaction_intf_all
       WHERE status = 'ELIGIBLE';
  
    CURSOR c2_cur(g_transaction_source     VARCHAR2,
                  g_transaction_batch_name VARCHAR2) IS
      SELECT *
        FROM gets_pa_transaction_intf_all
       WHERE status = 'ELIGIBLE'
       ORDER BY transaction_source, batch_name, project_id, task_id;
    v_pa_std_intf_rec  pa_transaction_interface_all%ROWTYPE;
    v_error_count      NUMBER;
    v_txn_interface_id pa_transaction_interface_all.txn_interface_id%TYPE;
  BEGIN
    errbuf  := 'Sucessfully Completed';
    retcode := 0;
    dbms_output.put_line('ELIGIBLE_PA_BATCH start=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'ELIGIBLE_PA_BATCH start=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
  
    FOR c1_rec IN c1_cur LOOP
      v_error_count := 0;
      fnd_file.put_line(fnd_file.LOG,
                        'Start processing for Transaction Source: ' ||
                        c1_rec.transaction_source || ' and Batch: ' ||
                        c1_rec.batch_name);
      FOR c2_rec IN c2_cur(c1_rec.transaction_source, c1_rec.batch_name) LOOP
        BEGIN
          v_pa_std_intf_rec.transaction_source            := c1_rec.transaction_source;
          v_pa_std_intf_rec.batch_name                    := c1_rec.batch_name;
          v_pa_std_intf_rec.organization_name             := c2_rec.organization_name;
          v_pa_std_intf_rec.expenditure_item_date         := c2_rec.expenditure_item_date;
          v_pa_std_intf_rec.expenditure_ending_date       := c2_rec.expenditure_ending_date;
          v_pa_std_intf_rec.project_number                := c2_rec.project_number;
          v_pa_std_intf_rec.expenditure_type              := c2_rec.expenditure_type;
          v_pa_std_intf_rec.quantity                      := c2_rec.quantity;
          v_pa_std_intf_rec.raw_cost                      := c2_rec.raw_cost;
          v_pa_std_intf_rec.burdened_cost                 := c2_rec.burdened_cost;
          v_pa_std_intf_rec.transaction_status_code       := c2_rec.transaction_status_code;
          v_pa_std_intf_rec.project_id                    := c2_rec.project_id;
          v_pa_std_intf_rec.task_number                   := c2_rec.task_number;
          v_pa_std_intf_rec.task_id                       := c2_rec.task_id;
          v_pa_std_intf_rec.system_linkage                := c2_rec.system_linkage;
          v_pa_std_intf_rec.unmatched_negative_txn_flag   := c2_rec.unmatched_negative_txn_flag;
          v_pa_std_intf_rec.org_id                        := c2_rec.org_id;
          v_txn_interface_id                              := pa_txn_interface_s.NEXTVAL;
          v_pa_std_intf_rec.txn_interface_id              := v_txn_interface_id;
          v_pa_std_intf_rec.created_by                    := c2_rec.created_by;
          v_pa_std_intf_rec.creation_date                 := c2_rec.creation_date;
          v_pa_std_intf_rec.last_updated_by               := c2_rec.last_updated_by;
          v_pa_std_intf_rec.last_update_date              := c2_rec.last_update_date;
          v_pa_std_intf_rec.expenditure_comment           := c2_rec.expenditure_comment;
          v_pa_std_intf_rec.interface_id                  := c2_rec.interface_id;
          v_pa_std_intf_rec.user_transaction_source       := c2_rec.user_transaction_source;
          v_pa_std_intf_rec.employee_number               := c2_rec.employee_number;
          v_pa_std_intf_rec.non_labor_resource            := c2_rec.non_labor_resource;
          v_pa_std_intf_rec.non_labor_resource_org_name   := c2_rec.non_labor_resource_org_name;
          v_pa_std_intf_rec.transaction_rejection_code    := c2_rec.transaction_rejection_code;
          v_pa_std_intf_rec.expenditure_id                := c2_rec.expenditure_id;
          v_pa_std_intf_rec.attribute_category            := c2_rec.attribute_category;
          v_pa_std_intf_rec.attribute1                    := c2_rec.attribute1;
          v_pa_std_intf_rec.attribute2                    := c2_rec.attribute2;
          v_pa_std_intf_rec.attribute3                    := NULL;
          v_pa_std_intf_rec.attribute4                    := NULL;
          v_pa_std_intf_rec.attribute5                    := NULL;
          v_pa_std_intf_rec.attribute6                    := NULL;
          v_pa_std_intf_rec.attribute7                    := NULL;
          v_pa_std_intf_rec.attribute8                    := c2_rec.attribute8;
          v_pa_std_intf_rec.attribute9                    := c2_rec.attribute9;
          v_pa_std_intf_rec.attribute10                   := c2_rec.attribute10;
          v_pa_std_intf_rec.raw_cost_rate                 := c2_rec.raw_cost_rate;
          v_pa_std_intf_rec.expenditure_item_id           := c2_rec.expenditure_item_id;
          v_pa_std_intf_rec.dr_code_combination_id        := c2_rec.dr_code_combination_id;
          v_pa_std_intf_rec.cr_code_combination_id        := c2_rec.cr_code_combination_id;
          v_pa_std_intf_rec.cdl_system_reference1         := c2_rec.cdl_system_reference1;
          v_pa_std_intf_rec.cdl_system_reference2         := c2_rec.cdl_system_reference2;
          v_pa_std_intf_rec.cdl_system_reference3         := c2_rec.cdl_system_reference3;
          v_pa_std_intf_rec.gl_date                       := c2_rec.gl_date;
          v_pa_std_intf_rec.burdened_cost_rate            := c2_rec.burdened_cost_rate;
          v_pa_std_intf_rec.receipt_currency_amount       := c2_rec.receipt_currency_amount;
          v_pa_std_intf_rec.receipt_currency_code         := c2_rec.receipt_currency_code;
          v_pa_std_intf_rec.receipt_exchange_rate         := c2_rec.receipt_exchange_rate;
          v_pa_std_intf_rec.denom_currency_code           := c2_rec.denom_currency_code;
          v_pa_std_intf_rec.denom_raw_cost                := c2_rec.denom_raw_cost;
          v_pa_std_intf_rec.denom_burdened_cost           := c2_rec.denom_burdened_cost;
          v_pa_std_intf_rec.acct_rate_date                := c2_rec.acct_rate_date;
          v_pa_std_intf_rec.acct_rate_type                := c2_rec.acct_rate_type;
          v_pa_std_intf_rec.acct_exchange_rate            := c2_rec.acct_exchange_rate;
          v_pa_std_intf_rec.acct_raw_cost                 := c2_rec.acct_raw_cost;
          v_pa_std_intf_rec.acct_burdened_cost            := c2_rec.acct_burdened_cost;
          v_pa_std_intf_rec.acct_exchange_rounding_limit  := c2_rec.acct_exchange_rounding_limit;
          v_pa_std_intf_rec.project_currency_code         := c2_rec.project_currency_code;
          v_pa_std_intf_rec.project_rate_date             := c2_rec.project_rate_date;
          v_pa_std_intf_rec.project_rate_type             := c2_rec.project_rate_type;
          v_pa_std_intf_rec.project_exchange_rate         := c2_rec.project_exchange_rate;
          v_pa_std_intf_rec.orig_exp_txn_reference1       := c2_rec.orig_exp_txn_reference1;
          v_pa_std_intf_rec.orig_exp_txn_reference2       := c2_rec.orig_exp_txn_reference2;
          v_pa_std_intf_rec.orig_exp_txn_reference3       := c2_rec.orig_exp_txn_reference3;
          v_pa_std_intf_rec.orig_user_exp_txn_reference   := c2_rec.orig_user_exp_txn_reference;
          v_pa_std_intf_rec.override_to_organization_name := c2_rec.override_to_organization_name;
          v_pa_std_intf_rec.reversed_orig_txn_reference   := c2_rec.reversed_orig_txn_reference;
          v_pa_std_intf_rec.billable_flag                 := c2_rec.billable_flag;
          v_pa_std_intf_rec.person_business_group_name    := c2_rec.person_business_group_name;
          v_pa_std_intf_rec.projfunc_currency_code        := c2_rec.projfunc_currency_code;
          v_pa_std_intf_rec.projfunc_cost_rate_type       := c2_rec.projfunc_cost_rate_type;
          v_pa_std_intf_rec.projfunc_cost_rate_date       := c2_rec.projfunc_cost_rate_date;
          v_pa_std_intf_rec.projfunc_cost_exchange_rate   := c2_rec.projfunc_cost_exchange_rate;
          v_pa_std_intf_rec.project_raw_cost              := c2_rec.project_raw_cost;
          v_pa_std_intf_rec.project_burdened_cost         := c2_rec.project_burdened_cost;
          v_pa_std_intf_rec.assignment_name               := c2_rec.assignment_name;
          v_pa_std_intf_rec.work_type_name                := c2_rec.work_type_name;
          v_pa_std_intf_rec.cdl_system_reference4         := c2_rec.cdl_system_reference4;
          v_pa_std_intf_rec.accrual_flag                  := c2_rec.accrual_flag;
          v_pa_std_intf_rec.person_id                     := c2_rec.person_id;
          v_pa_std_intf_rec.organization_id               := c2_rec.organization_id;
          v_pa_std_intf_rec.non_labor_resource_org_id     := c2_rec.non_labor_resource_org_id;
          v_pa_std_intf_rec.override_to_organization_id   := c2_rec.override_to_organization_id;
          v_pa_std_intf_rec.assignment_id                 := c2_rec.assignment_id;
          v_pa_std_intf_rec.work_type_id                  := c2_rec.work_type_id;
          v_pa_std_intf_rec.person_business_group_id      := c2_rec.person_business_group_id;
          v_pa_std_intf_rec.inventory_item_id             := c2_rec.inventory_item_id;
          v_pa_std_intf_rec.wip_resource_id               := c2_rec.wip_resource_id;
          v_pa_std_intf_rec.unit_of_measure               := c2_rec.unit_of_measure;
          v_pa_std_intf_rec.po_number                     := c2_rec.po_number;
          v_pa_std_intf_rec.po_header_id                  := c2_rec.po_header_id;
          v_pa_std_intf_rec.po_line_num                   := c2_rec.po_line_num;
          v_pa_std_intf_rec.po_line_id                    := c2_rec.po_line_id;
          v_pa_std_intf_rec.person_type                   := c2_rec.person_type;
          v_pa_std_intf_rec.po_price_type                 := c2_rec.po_price_type;
          v_pa_std_intf_rec.vendor_id                     := c2_rec.vendor_id;
          v_pa_std_intf_rec.vendor_number                 := c2_rec.vendor_number;
          v_pa_std_intf_rec.orig_transaction_reference    := c2_rec.orig_transaction_reference;
        
          insert_into_std_interface(errbuf, retcode, v_pa_std_intf_rec);
        EXCEPTION
          WHEN OTHERS THEN
            errbuf := SQLERRM;
            BEGIN
              UPDATE gets_pa_transaction_intf_all
                 SET status           = 'ERROR',
                     status_message   = 'Error inserting into std interface=> ' ||
                                        errbuf,
                     interface_id     = v_txn_interface_id,
                     last_update_date = SYSDATE,
                     last_updated_by  = fnd_global.user_id
               WHERE org_id = c2_rec.org_id
                 AND project_id = c2_rec.project_id
                 AND task_id = c2_rec.task_id
                 AND transaction_source = c1_rec.transaction_source
                 AND batch_name = c1_rec.batch_name
                 AND expenditure_type = c2_rec.expenditure_type
                 AND orig_transaction_reference =
                     c2_rec.orig_transaction_reference;
            EXCEPTION
              WHEN OTHERS THEN
                errbuf  := SUBSTR('Error updating gets_pa_transaction_intf_all for ERROR1, error=>' ||
                                  SQLERRM,
                                  1,
                                  255);
                retcode := 2;
                ROLLBACK;
                RETURN;
            END;
        END;
        -- Update ready for the successful transfer to std interface
        BEGIN
          UPDATE gets_pa_transaction_intf_all
             SET status           = 'READY',
                 last_update_date = SYSDATE,
                 last_updated_by  = fnd_global.user_id
           WHERE org_id = c2_rec.org_id
             AND project_id = c2_rec.project_id
             AND task_id = c2_rec.task_id
             AND transaction_source = c1_rec.transaction_source
             AND batch_name = c1_rec.batch_name
             AND expenditure_type = c2_rec.expenditure_type
             AND orig_transaction_reference =
                 c2_rec.orig_transaction_reference;
        EXCEPTION
          WHEN OTHERS THEN
            errbuf  := SUBSTR('Error updating gets_pa_transaction_intf_all for READY, error=>' ||
                              SQLERRM,
                              1,
                              255);
            retcode := 2;
            ROLLBACK;
            RETURN;
        END;
      END LOOP;
      -- Check for any error during std interface transfer
      BEGIN
        SELECT COUNT(1)
          INTO v_error_count
          FROM gets_pa_transaction_intf_all
         WHERE transaction_source = c1_rec.transaction_source
           AND batch_name = c1_rec.batch_name
           AND status = 'ERROR';
      EXCEPTION
        WHEN OTHERS THEN
          v_error_count := 0;
      END;
      IF v_error_count > 0 THEN
        BEGIN
          UPDATE gets_pa_transaction_intf_all
             SET status           = 'ERROR',
                 status_message   = nvl(status_message,
                                        'Other lines of same batch are in error'),
                 last_update_date = SYSDATE,
                 last_updated_by  = fnd_global.user_id
           WHERE transaction_source = c1_rec.transaction_source
             AND batch_name = c1_rec.batch_name;
        EXCEPTION
          WHEN OTHERS THEN
            errbuf  := SUBSTR('Error updating gets_pa_transaction_intf_all for ERROR2, error=>' ||
                              SQLERRM,
                              1,
                              255);
            retcode := 2;
            ROLLBACK;
            RETURN;
        END;
      ELSE
        -- Calling PRC: Transaction Import for unique Transaction Source and Batch in one run
        run_pa_import(errbuf               => errbuf,
                      retcode              => retcode,
                      p_transaction_source => c1_rec.transaction_source,
                      p_batch_name         => c1_rec.batch_name);
        IF retcode <> 0 THEN
          DELETE FROM pa_transaction_interface_all
           WHERE transaction_source = c1_rec.transaction_source
             AND batch_name = c1_rec.batch_name;
          COMMIT;
          retcode := 2;
          errbuf  := 'Error calling run_pa_import=> ';
          ROLLBACK;
          RETURN;
        END IF;
      END IF;
    END LOOP;
    COMMIT;
    dbms_output.put_line('ELIGIBLE_PA_BATCH completed=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'ELIGIBLE_PA_BATCH completed=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := SUBSTR('Unexpected Error in ELIGIBLE_PA_BATCH procedure, error=>' ||
                        SQLERRM,
                        1,
                        255);
      retcode := 2;
      ROLLBACK;
      RETURN;
  END eligible_pa_batch;
  -----------------------------------------------------------------------------------------------------------------------
  -- Function to get cost and revenue budget associated with project
  -----------------------------------------------------------------------------------------------------------------------
  FUNCTION get_budget_details(p_project_id  IN NUMBER,
                              p_task_id     IN NUMBER,
                              p_budget_type IN VARCHAR2) RETURN NUMBER IS
    v_budget_amt       NUMBER := 0;
    v_budget_type_code VARCHAR2(25);
  BEGIN
    BEGIN
      SELECT DISTINCT budget_type_code
        INTO v_budget_type_code
        FROM pa_budget_versions pbv
       WHERE pbv.project_id = p_project_id
         AND budget_type_code = p_budget_type;
    EXCEPTION
      WHEN OTHERS THEN
        v_budget_type_code := NULL;
    END;
    IF v_budget_type_code IS NOT NULL THEN
      IF v_budget_type_code = 'AC' THEN
        BEGIN
          SELECT NVL(SUM(NVL(pbl.raw_cost, 0)), 0)
            INTO v_budget_amt
            FROM pa_budget_versions      pbv,
                 pa_budget_lines         pbl,
                 pa_resource_assignments pra
           WHERE pbv.budget_version_id = pbl.budget_version_id
             AND pbl.resource_assignment_id = pra.resource_assignment_id
             AND pra.task_id = (SELECT task_id
                                  FROM apps.pa_tasks
                                 WHERE project_id = p_project_id
                                   AND task_id = p_task_id)
             AND pbv.project_id = p_project_id
             AND budget_status_code = 'B'
             AND budget_type_code = v_budget_type_code
             AND version_number =
                 (SELECT MAX(version_number)
                    FROM pa_budget_versions pb1
                   WHERE pb1.project_id = p_project_id
                     AND pb1.budget_status_code = 'B'
                     AND pb1.budget_type_code = v_budget_type_code);
        EXCEPTION
          WHEN OTHERS THEN
            v_budget_amt := 0;
        END;
      ELSE
        BEGIN
          SELECT NVL(SUM(NVL(pbv.revenue, 0)), 0)
            INTO v_budget_amt
            FROM pa_budget_versions pbv
           WHERE pbv.project_id = p_project_id
             AND budget_status_code = 'B'
             AND budget_type_code = v_budget_type_code
             AND version_number =
                 (SELECT MAX(version_number)
                    FROM pa_budget_versions pb1
                   WHERE pb1.project_id = p_project_id
                     AND pb1.budget_status_code = 'B'
                     AND pb1.budget_type_code = v_budget_type_code);
        EXCEPTION
          WHEN OTHERS THEN
            v_budget_amt := 0;
        END;
      END IF;
    ELSE
      v_budget_amt := 0;
    END IF;
    RETURN v_budget_amt;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END get_budget_details;
  -----------------------------------------------------------------------------------------------------------------------
  -- Function to get revenue totals for a project
  -----------------------------------------------------------------------------------------------------------------------
  FUNCTION get_revenue_totals(p_project_id IN pa_projects_all.project_id%TYPE)
    RETURN NUMBER IS
    v_revenue_total_amount NUMBER := 0;
  BEGIN
    SELECT NVL(SUM(NVL(ri.amount, 0)), 0)
      INTO v_revenue_total_amount
      FROM pa_draft_revenues_all  r,
           pa_draft_revenue_items ri,
           pa_lookups             lk
     WHERE r.project_id = p_project_id
       AND ri.project_id = r.project_id
       AND ri.draft_revenue_num = r.draft_revenue_num
       AND lk.lookup_type = 'INVOICE/REVENUE STATUS'
       AND lk.lookup_code =
           DECODE(r.generation_error_flag,
                  'Y',
                  'GENERATION ERROR',
                  DECODE(r.released_date,
                         NULL,
                         'UNRELEASED',
                         DECODE(r.transfer_status_code,
                                'P',
                                'RELEASED',
                                'X',
                                'REJECTED IN TRANSFER',
                                'T',
                                'TRANSFERRED',
                                'A',
                                'ACCEPTED',
                                'R',
                                'REJECTED',
                                'RECEIVED')))
       AND lk.meaning IN ('Released', 'Accepted');
    RETURN v_revenue_total_amount;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END get_revenue_totals;
  -----------------------------------------------------------------------------------------------------------------------
  -- Function to get AR revenue for a project
  -----------------------------------------------------------------------------------------------------------------------
  FUNCTION get_ar_revenue(p_org_id         IN NUMBER,
                          p_project_number IN pa_projects_all.segment1%TYPE)
    RETURN NUMBER IS
    v_ar_revenue NUMBER := 0;
  BEGIN
    SELECT nvl(SUM(nvl(ra_dist.amount, 0)), 0)
      INTO v_ar_revenue
      FROM apps.ra_customer_trx_all          rcta,
           apps.ra_cust_trx_line_gl_dist_all ra_dist,
           apps.ra_batch_sources_all         rbsa
     WHERE rcta.customer_trx_id = ra_dist.customer_trx_id
       AND rbsa.batch_source_id = rcta.batch_source_id
       AND rcta.org_id = p_org_id
       AND rcta.global_attribute27 = p_project_number
       AND ra_dist.account_class = 'REV'
       AND rcta.complete_flag = 'Y'
       AND rbsa.name <> g_trx_source
       AND NOT EXISTS
       (SELECT 1
             FROM fnd_lookup_values flv, RA_CUST_TRX_TYPES_all rctta
             WHERE 1 = 1
             AND flv.language = 'US'
             AND flv.lookup_type = 'GETS_PA_BILL_PSC_AR_REV_SOURCE'
             AND flv.enabled_flag = 'Y'
             AND flv.description = 'TYPE'
             AND flv.tag = 'EXCLUDE'
             AND upper(flv.lookup_code) = upper(rctta.name)
             AND rctta.cust_trx_type_id = rcta.cust_trx_type_id
             AND rctta.org_id = rcta.org_id
             AND nvl(flv.start_date_active, trunc(SYSDATE)) <= trunc(SYSDATE)
             AND nvl(flv.end_date_active, trunc(SYSDATE)) >= trunc(SYSDATE));
    RETURN v_ar_revenue;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END get_ar_revenue;
  -----------------------------------------------------------------------------------------------------------------------
  -- Function to get accumulated warranty reserve
  -----------------------------------------------------------------------------------------------------------------------
  FUNCTION get_accm_warranty(p_org_id             gets_inv_op_unit_info.org_id%TYPE,
                             p_project_id         IN pa_projects_all.project_id%TYPE,
                             p_task_id            IN pa_tasks.task_id%TYPE,
                             p_expenditure_type   gets_pa_transaction_intf_all.expenditure_type%TYPE,
                             p_transaction_source gets_pa_transaction_intf_all.transaction_source%TYPE)
    RETURN NUMBER IS
    v_accum_warranty_reserve        NUMBER := 0;
    v_accum_warranty_reserve_prg    NUMBER := 0;
    v_accum_warranty_reserve_manual NUMBER := 0;
  BEGIN
    SELECT nvl(round(sum(to_number(nvl(attribute10, 0))), 2), 0)
      INTO v_accum_warranty_reserve_prg
      FROM gets_pa_transaction_intf_all
     WHERE org_id = p_org_id
       AND project_id = p_project_id
       AND task_id = p_task_id
       AND expenditure_type = p_expenditure_type
       AND transaction_source = p_transaction_source
       AND status = 'CREATED';
    SELECT nvl(round(sum(to_number(nvl(raw_cost, 0))), 2), 0)
      INTO v_accum_warranty_reserve_manual
      FROM pa_expenditure_items_all
     WHERE org_id = p_org_id
       AND project_id = p_project_id
       AND task_id = p_task_id
       AND expenditure_type = p_expenditure_type
       AND transaction_source = p_transaction_source
       AND nvl(attribute1, 'X') <>
           (SELECT segment1
              FROM pa_projects_all
             WHERE project_id = p_project_id);
    v_accum_warranty_reserve := v_accum_warranty_reserve_prg +
                                v_accum_warranty_reserve_manual;
    RETURN v_accum_warranty_reserve;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END get_accm_warranty;
  -----------------------------------------------------------------------------------------------------------------------
  -- Function to get PA EXP Org
  -----------------------------------------------------------------------------------------------------------------------
  FUNCTION get_project_org(p_project_id pa_projects_all.project_id%TYPE)
    RETURN VARCHAR2 IS
    v_project_org hr_all_organization_units.name%TYPE;
  BEGIN
    SELECT hou.name
      INTO v_project_org
      FROM pa_projects_all pa, hr_all_organization_units hou
     WHERE pa.carrying_out_organization_id = hou.organization_id
       AND pa.PROJECT_ID = p_project_id;
    RETURN v_project_org;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END get_project_org;
  -----------------------------------------------------------------------------------------------------------------------
  -- Function to get accumulated warranty reserve
  -----------------------------------------------------------------------------------------------------------------------
  FUNCTION get_orig_transaction_reference(p_org_id             gets_inv_op_unit_info.org_id%TYPE,
                                          p_project_id         IN pa_projects_all.project_id%TYPE,
                                          p_task_id            IN pa_tasks.task_id%TYPE,
                                          p_expenditure_type   gets_pa_transaction_intf_all.expenditure_type%TYPE,
                                          p_transaction_source gets_pa_transaction_intf_all.transaction_source%TYPE)
    RETURN NUMBER IS
    v_orig_transaction_reference gets_pa_transaction_intf_all.orig_transaction_reference%TYPE;
  BEGIN
    SELECT MAX(orig_transaction_reference)
      INTO v_orig_transaction_reference
      FROM gets_pa_transaction_intf_all
     WHERE org_id = p_org_id
       AND project_id = p_project_id
       AND task_id = p_task_id
       AND expenditure_type = p_expenditure_type
       AND transaction_source = p_transaction_source
       AND status = 'ERROR';
    RETURN v_orig_transaction_reference;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END get_orig_transaction_reference;
  -----------------------------------------------------------------------------------------------------------------------
  -- Procedure to insert into revenue custom PA interface table
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE gets_pa_interface_ins(errbuf          OUT VARCHAR2,
                                  retcode         OUT NUMBER,
                                  p_pa_custom_rec gets_pa_transaction_intf_all%ROWTYPE) IS
  BEGIN
    errbuf  := 'Sucessfully Completed';
    retcode := 0;
  
    INSERT INTO gets_pa_transaction_intf_all
      (transaction_source,
       batch_name,
       expenditure_ending_date,
       employee_number,
       organization_name,
       expenditure_item_date,
       project_number,
       task_number,
       expenditure_type,
       non_labor_resource,
       non_labor_resource_org_name,
       quantity,
       raw_cost,
       expenditure_comment,
       transaction_status_code,
       transaction_rejection_code,
       expenditure_id,
       orig_transaction_reference,
       attribute_category,
       attribute1,
       attribute2,
       attribute3,
       attribute4,
       attribute5,
       attribute6,
       attribute7,
       attribute8,
       attribute9,
       attribute10,
       raw_cost_rate,
       interface_id,
       unmatched_negative_txn_flag,
       expenditure_item_id,
       org_id,
       dr_code_combination_id,
       cr_code_combination_id,
       cdl_system_reference1,
       cdl_system_reference2,
       cdl_system_reference3,
       gl_date,
       burdened_cost,
       burdened_cost_rate,
       system_linkage,
       user_transaction_source,
       created_by,
       creation_date,
       last_updated_by,
       last_update_date,
       receipt_currency_amount,
       receipt_currency_code,
       receipt_exchange_rate,
       denom_currency_code,
       denom_raw_cost,
       denom_burdened_cost,
       acct_rate_date,
       acct_rate_type,
       acct_exchange_rate,
       acct_raw_cost,
       acct_burdened_cost,
       acct_exchange_rounding_limit,
       project_currency_code,
       project_rate_date,
       project_rate_type,
       project_exchange_rate,
       orig_exp_txn_reference1,
       orig_exp_txn_reference2,
       orig_exp_txn_reference3,
       orig_user_exp_txn_reference,
       vendor_number,
       override_to_organization_name,
       reversed_orig_txn_reference,
       billable_flag,
       person_business_group_name,
       projfunc_currency_code,
       projfunc_cost_rate_type,
       projfunc_cost_rate_date,
       projfunc_cost_exchange_rate,
       project_raw_cost,
       project_burdened_cost,
       assignment_name,
       work_type_name,
       cdl_system_reference4,
       accrual_flag,
       project_id,
       task_id,
       person_id,
       organization_id,
       non_labor_resource_org_id,
       vendor_id,
       override_to_organization_id,
       assignment_id,
       work_type_id,
       person_business_group_id,
       inventory_item_id,
       wip_resource_id,
       unit_of_measure,
       po_number,
       po_header_id,
       po_line_num,
       po_line_id,
       person_type,
       po_price_type,
       status,
       status_message)
    VALUES
      (p_pa_custom_rec.transaction_source,
       p_pa_custom_rec.batch_name,
       p_pa_custom_rec.expenditure_ending_date,
       p_pa_custom_rec.employee_number,
       p_pa_custom_rec.organization_name,
       p_pa_custom_rec.expenditure_item_date,
       p_pa_custom_rec.project_number,
       p_pa_custom_rec.task_number,
       p_pa_custom_rec.expenditure_type,
       p_pa_custom_rec.non_labor_resource,
       p_pa_custom_rec.non_labor_resource_org_name,
       p_pa_custom_rec.quantity,
       p_pa_custom_rec.raw_cost,
       p_pa_custom_rec.expenditure_comment,
       p_pa_custom_rec.transaction_status_code,
       p_pa_custom_rec.transaction_rejection_code,
       p_pa_custom_rec.expenditure_id,
       p_pa_custom_rec.orig_transaction_reference,
       p_pa_custom_rec.attribute_category,
       p_pa_custom_rec.attribute1,
       p_pa_custom_rec.attribute2,
       p_pa_custom_rec.attribute3,
       p_pa_custom_rec.attribute4,
       p_pa_custom_rec.attribute5,
       p_pa_custom_rec.attribute6,
       p_pa_custom_rec.attribute7,
       p_pa_custom_rec.attribute8,
       p_pa_custom_rec.attribute9,
       p_pa_custom_rec.attribute10,
       p_pa_custom_rec.raw_cost_rate,
       p_pa_custom_rec.interface_id,
       p_pa_custom_rec.unmatched_negative_txn_flag,
       p_pa_custom_rec.expenditure_item_id,
       p_pa_custom_rec.org_id,
       p_pa_custom_rec.dr_code_combination_id,
       p_pa_custom_rec.cr_code_combination_id,
       p_pa_custom_rec.cdl_system_reference1,
       p_pa_custom_rec.cdl_system_reference2,
       p_pa_custom_rec.cdl_system_reference3,
       p_pa_custom_rec.gl_date,
       p_pa_custom_rec.burdened_cost,
       p_pa_custom_rec.burdened_cost_rate,
       p_pa_custom_rec.system_linkage,
       p_pa_custom_rec.user_transaction_source,
       p_pa_custom_rec.created_by,
       p_pa_custom_rec.creation_date,
       p_pa_custom_rec.last_updated_by,
       p_pa_custom_rec.last_update_date,
       p_pa_custom_rec.receipt_currency_amount,
       p_pa_custom_rec.receipt_currency_code,
       p_pa_custom_rec.receipt_exchange_rate,
       p_pa_custom_rec.denom_currency_code,
       p_pa_custom_rec.denom_raw_cost,
       p_pa_custom_rec.denom_burdened_cost,
       p_pa_custom_rec.acct_rate_date,
       p_pa_custom_rec.acct_rate_type,
       p_pa_custom_rec.acct_exchange_rate,
       p_pa_custom_rec.acct_raw_cost,
       p_pa_custom_rec.acct_burdened_cost,
       p_pa_custom_rec.acct_exchange_rounding_limit,
       p_pa_custom_rec.project_currency_code,
       p_pa_custom_rec.project_rate_date,
       p_pa_custom_rec.project_rate_type,
       p_pa_custom_rec.project_exchange_rate,
       p_pa_custom_rec.orig_exp_txn_reference1,
       p_pa_custom_rec.orig_exp_txn_reference2,
       p_pa_custom_rec.orig_exp_txn_reference3,
       p_pa_custom_rec.orig_user_exp_txn_reference,
       p_pa_custom_rec.vendor_number,
       p_pa_custom_rec.override_to_organization_name,
       p_pa_custom_rec.reversed_orig_txn_reference,
       p_pa_custom_rec.billable_flag,
       p_pa_custom_rec.person_business_group_name,
       p_pa_custom_rec.projfunc_currency_code,
       p_pa_custom_rec.projfunc_cost_rate_type,
       p_pa_custom_rec.projfunc_cost_rate_date,
       p_pa_custom_rec.projfunc_cost_exchange_rate,
       p_pa_custom_rec.project_raw_cost,
       p_pa_custom_rec.project_burdened_cost,
       p_pa_custom_rec.assignment_name,
       p_pa_custom_rec.work_type_name,
       p_pa_custom_rec.cdl_system_reference4,
       p_pa_custom_rec.accrual_flag,
       p_pa_custom_rec.project_id,
       p_pa_custom_rec.task_id,
       p_pa_custom_rec.person_id,
       p_pa_custom_rec.organization_id,
       p_pa_custom_rec.non_labor_resource_org_id,
       p_pa_custom_rec.vendor_id,
       p_pa_custom_rec.override_to_organization_id,
       p_pa_custom_rec.assignment_id,
       p_pa_custom_rec.work_type_id,
       p_pa_custom_rec.person_business_group_id,
       p_pa_custom_rec.inventory_item_id,
       p_pa_custom_rec.wip_resource_id,
       p_pa_custom_rec.unit_of_measure,
       p_pa_custom_rec.po_number,
       p_pa_custom_rec.po_header_id,
       p_pa_custom_rec.po_line_num,
       p_pa_custom_rec.po_line_id,
       p_pa_custom_rec.person_type,
       p_pa_custom_rec.po_price_type,
       p_pa_custom_rec.status,
       p_pa_custom_rec.status_message);
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := 'Error while Inserting in gets_pa_transaction_intf_all => ' ||
                 SQLERRM;
      retcode := 2;
      ROLLBACK;
      RETURN;
  END gets_pa_interface_ins;
  -----------------------------------------------------------------------------------------------------------------------
  -- Procedure to update into revenue custom PA interface table
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE gets_pa_interface_upd(errbuf              OUT VARCHAR2,
                                  retcode             OUT NUMBER,
                                  p_pa_custom_rec_upd gets_pa_transaction_intf_all%ROWTYPE) IS
  BEGIN
    errbuf  := 'Sucessfully Completed';
    retcode := 0;
  
    UPDATE gets_pa_transaction_intf_all
       SET batch_name                  = p_pa_custom_rec_upd.batch_name,
           expenditure_ending_date     = p_pa_custom_rec_upd.expenditure_ending_date,
           expenditure_item_date       = p_pa_custom_rec_upd.expenditure_item_date,
           quantity                    = p_pa_custom_rec_upd.quantity,
           raw_cost                    = p_pa_custom_rec_upd.raw_cost,
           burdened_cost               = p_pa_custom_rec_upd.burdened_cost,
           transaction_status_code     = p_pa_custom_rec_upd.transaction_status_code,
           transaction_rejection_code  = p_pa_custom_rec_upd.transaction_rejection_code,
           unmatched_negative_txn_flag = p_pa_custom_rec_upd.unmatched_negative_txn_flag,
           attribute1                  = p_pa_custom_rec_upd.attribute1,
           attribute2                  = p_pa_custom_rec_upd.attribute2,
           attribute3                  = p_pa_custom_rec_upd.attribute3,
           attribute4                  = p_pa_custom_rec_upd.attribute4,
           attribute5                  = p_pa_custom_rec_upd.attribute5,
           attribute6                  = p_pa_custom_rec_upd.attribute6,
           attribute7                  = p_pa_custom_rec_upd.attribute7,
           attribute8                  = p_pa_custom_rec_upd.attribute8,
           attribute9                  = p_pa_custom_rec_upd.attribute9,
           attribute10                 = p_pa_custom_rec_upd.attribute10,
           denom_raw_cost              = p_pa_custom_rec_upd.denom_raw_cost,
           status                      = p_pa_custom_rec_upd.status,
           status_message              = p_pa_custom_rec_upd.status_message,
           last_update_date            = SYSDATE,
           last_updated_by             = fnd_global.user_id
     WHERE org_id = p_pa_custom_rec_upd.org_id
       AND project_id = p_pa_custom_rec_upd.project_id
       AND task_id = p_pa_custom_rec_upd.task_id
       AND transaction_source = p_pa_custom_rec_upd.transaction_source
       AND expenditure_type = p_pa_custom_rec_upd.expenditure_type
       AND orig_transaction_reference =
           p_pa_custom_rec_upd.orig_transaction_reference
       AND status = 'ERROR';
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := 'Error while Updating in gets_pa_transaction_intf_all for ELIGIBLE => ' ||
                 SQLERRM;
      retcode := 2;
      ROLLBACK;
      RETURN;
  END gets_pa_interface_upd;
  -----------------------------------------------------------------------------------------------------------------------
  -- Procedure to insert into std PA interface table
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE insert_into_std_interface(errbuf       OUT VARCHAR2,
                                      retcode      OUT NUMBER,
                                      p_pa_std_rec pa_transaction_interface_all%ROWTYPE) IS
  BEGIN
    errbuf  := 'Sucessfully Completed';
    retcode := 0;
  
    INSERT INTO pa_transaction_interface_all
      (transaction_source,
       batch_name,
       expenditure_ending_date,
       employee_number,
       organization_name,
       expenditure_item_date,
       project_number,
       task_number,
       expenditure_type,
       non_labor_resource,
       non_labor_resource_org_name,
       quantity,
       raw_cost,
       expenditure_comment,
       transaction_status_code,
       transaction_rejection_code,
       expenditure_id,
       orig_transaction_reference,
       attribute_category,
       attribute1,
       attribute2,
       attribute3,
       attribute4,
       attribute5,
       attribute6,
       attribute7,
       attribute8,
       attribute9,
       attribute10,
       raw_cost_rate,
       interface_id,
       unmatched_negative_txn_flag,
       expenditure_item_id,
       org_id,
       dr_code_combination_id,
       cr_code_combination_id,
       cdl_system_reference1,
       cdl_system_reference2,
       cdl_system_reference3,
       gl_date,
       burdened_cost,
       burdened_cost_rate,
       system_linkage,
       txn_interface_id,
       user_transaction_source,
       created_by,
       creation_date,
       last_updated_by,
       last_update_date,
       receipt_currency_amount,
       receipt_currency_code,
       receipt_exchange_rate,
       denom_currency_code,
       denom_raw_cost,
       denom_burdened_cost,
       acct_rate_date,
       acct_rate_type,
       acct_exchange_rate,
       acct_raw_cost,
       acct_burdened_cost,
       acct_exchange_rounding_limit,
       project_currency_code,
       project_rate_date,
       project_rate_type,
       project_exchange_rate,
       orig_exp_txn_reference1,
       orig_exp_txn_reference2,
       orig_exp_txn_reference3,
       orig_user_exp_txn_reference,
       vendor_number,
       override_to_organization_name,
       reversed_orig_txn_reference,
       billable_flag,
       person_business_group_name,
       projfunc_currency_code,
       projfunc_cost_rate_type,
       projfunc_cost_rate_date,
       projfunc_cost_exchange_rate,
       project_raw_cost,
       project_burdened_cost,
       assignment_name,
       work_type_name,
       cdl_system_reference4,
       accrual_flag,
       project_id,
       task_id,
       person_id,
       organization_id,
       non_labor_resource_org_id,
       vendor_id,
       override_to_organization_id,
       assignment_id,
       work_type_id,
       person_business_group_id,
       inventory_item_id,
       wip_resource_id,
       unit_of_measure,
       po_number,
       po_header_id,
       po_line_num,
       po_line_id,
       person_type,
       po_price_type)
    VALUES
      (p_pa_std_rec.transaction_source,
       p_pa_std_rec.batch_name,
       p_pa_std_rec.expenditure_ending_date,
       p_pa_std_rec.employee_number,
       p_pa_std_rec.organization_name,
       p_pa_std_rec.expenditure_item_date,
       p_pa_std_rec.project_number,
       p_pa_std_rec.task_number,
       p_pa_std_rec.expenditure_type,
       p_pa_std_rec.non_labor_resource,
       p_pa_std_rec.non_labor_resource_org_name,
       p_pa_std_rec.quantity,
       p_pa_std_rec.raw_cost,
       p_pa_std_rec.expenditure_comment,
       p_pa_std_rec.transaction_status_code,
       p_pa_std_rec.transaction_rejection_code,
       p_pa_std_rec.expenditure_id,
       p_pa_std_rec.orig_transaction_reference,
       p_pa_std_rec.attribute_category,
       p_pa_std_rec.attribute1,
       p_pa_std_rec.attribute2,
       p_pa_std_rec.attribute3,
       p_pa_std_rec.attribute4,
       p_pa_std_rec.attribute5,
       p_pa_std_rec.attribute6,
       p_pa_std_rec.attribute7,
       p_pa_std_rec.attribute8,
       p_pa_std_rec.attribute9,
       p_pa_std_rec.attribute10,
       p_pa_std_rec.raw_cost_rate,
       p_pa_std_rec.interface_id,
       p_pa_std_rec.unmatched_negative_txn_flag,
       p_pa_std_rec.expenditure_item_id,
       p_pa_std_rec.org_id,
       p_pa_std_rec.dr_code_combination_id,
       p_pa_std_rec.cr_code_combination_id,
       p_pa_std_rec.cdl_system_reference1,
       p_pa_std_rec.cdl_system_reference2,
       p_pa_std_rec.cdl_system_reference3,
       p_pa_std_rec.gl_date,
       p_pa_std_rec.burdened_cost,
       p_pa_std_rec.burdened_cost_rate,
       p_pa_std_rec.system_linkage,
       p_pa_std_rec.txn_interface_id,
       p_pa_std_rec.user_transaction_source,
       p_pa_std_rec.created_by,
       p_pa_std_rec.creation_date,
       p_pa_std_rec.last_updated_by,
       p_pa_std_rec.last_update_date,
       p_pa_std_rec.receipt_currency_amount,
       p_pa_std_rec.receipt_currency_code,
       p_pa_std_rec.receipt_exchange_rate,
       p_pa_std_rec.denom_currency_code,
       p_pa_std_rec.denom_raw_cost,
       p_pa_std_rec.denom_burdened_cost,
       p_pa_std_rec.acct_rate_date,
       p_pa_std_rec.acct_rate_type,
       p_pa_std_rec.acct_exchange_rate,
       p_pa_std_rec.acct_raw_cost,
       p_pa_std_rec.acct_burdened_cost,
       p_pa_std_rec.acct_exchange_rounding_limit,
       p_pa_std_rec.project_currency_code,
       p_pa_std_rec.project_rate_date,
       p_pa_std_rec.project_rate_type,
       p_pa_std_rec.project_exchange_rate,
       p_pa_std_rec.orig_exp_txn_reference1,
       p_pa_std_rec.orig_exp_txn_reference2,
       p_pa_std_rec.orig_exp_txn_reference3,
       p_pa_std_rec.orig_user_exp_txn_reference,
       p_pa_std_rec.vendor_number,
       p_pa_std_rec.override_to_organization_name,
       p_pa_std_rec.reversed_orig_txn_reference,
       p_pa_std_rec.billable_flag,
       p_pa_std_rec.person_business_group_name,
       p_pa_std_rec.projfunc_currency_code,
       p_pa_std_rec.projfunc_cost_rate_type,
       p_pa_std_rec.projfunc_cost_rate_date,
       p_pa_std_rec.projfunc_cost_exchange_rate,
       p_pa_std_rec.project_raw_cost,
       p_pa_std_rec.project_burdened_cost,
       p_pa_std_rec.assignment_name,
       p_pa_std_rec.work_type_name,
       p_pa_std_rec.cdl_system_reference4,
       p_pa_std_rec.accrual_flag,
       p_pa_std_rec.project_id,
       p_pa_std_rec.task_id,
       p_pa_std_rec.person_id,
       p_pa_std_rec.organization_id,
       p_pa_std_rec.non_labor_resource_org_id,
       p_pa_std_rec.vendor_id,
       p_pa_std_rec.override_to_organization_id,
       p_pa_std_rec.assignment_id,
       p_pa_std_rec.work_type_id,
       p_pa_std_rec.person_business_group_id,
       p_pa_std_rec.inventory_item_id,
       p_pa_std_rec.wip_resource_id,
       p_pa_std_rec.unit_of_measure,
       p_pa_std_rec.po_number,
       p_pa_std_rec.po_header_id,
       p_pa_std_rec.po_line_num,
       p_pa_std_rec.po_line_id,
       p_pa_std_rec.person_type,
       p_pa_std_rec.po_price_type);
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := 'Error while Inserting in pa_transaction_interface_all => ' ||
                 SQLERRM;
      retcode := 2;
      ROLLBACK;
      RETURN;
  END insert_into_std_interface;
  -----------------------------------------------------------------------------------------------------------------------
  -- Procedure to update call PRC: Transaction Import
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE run_pa_import(errbuf               OUT VARCHAR2,
                          retcode              OUT NUMBER,
                          p_transaction_source IN VARCHAR2,
                          p_batch_name         IN VARCHAR2) IS
    -- Get error info
    CURSOR c1_cur IS
      SELECT *
        FROM pa_transaction_interface_all
       WHERE transaction_source = p_transaction_source
         AND batch_name = p_batch_name;
    -- Get success info     
    CURSOR c2_cur IS
      SELECT peia.*, pec.expenditure_comment
        FROM pa_expenditure_items_all peia, pa_expenditure_comments pec
       WHERE peia.expenditure_item_id = pec.expenditure_item_id
         AND peia.transaction_source = p_transaction_source
         AND peia.attribute9 = p_batch_name;
    v_pa_import_request_id         NUMBER := 0;
    v_interval                     NUMBER := 2;
    v_max_wait                     NUMBER := 900;
    v_phase                        VARCHAR2(30);
    v_status                       VARCHAR2(30);
    v_pa_import_phase              VARCHAR2(30);
    v_pa_import_status             VARCHAR2(30);
    v_pa_import_message            VARCHAR2(1000);
    v_wait_result                  BOOLEAN;
    v_error_count                  NUMBER := 0;
    v_transaction_rejection_reason VARCHAR2(2000);
  BEGIN
    errbuf  := 'Sucessfully Completed';
    retcode := 0;
  
    fnd_global.apps_initialize(user_id      => g_user_id, --User Id
                               resp_id      => g_resp_id, --Responsibility Id
                               resp_appl_id => g_resp_app_id --101 --Responsibi lity Application Id
                               );
  
    BEGIN
      v_pa_import_request_id := fnd_request.submit_request('PA',
                                                           'PAXTRTRX',
                                                           '',
                                                           '',
                                                           NULL,
                                                           p_transaction_source,
                                                           p_batch_name);
    EXCEPTION
      WHEN OTHERS THEN
        v_pa_import_request_id := -1;
    END;
  
    IF NVL(v_pa_import_request_id, 0) > 0 THEN
      COMMIT;
      fnd_file.put_line(fnd_file.log,
                        'PA Import Req Id =>' || v_pa_import_request_id);
      v_wait_result := FND_CONCURRENT.WAIT_FOR_REQUEST(v_pa_import_request_id,
                                                       v_interval,
                                                       v_max_wait,
                                                       v_phase,
                                                       v_status,
                                                       v_pa_import_phase,
                                                       v_pa_import_status,
                                                       v_pa_import_message);
    ELSE
      fnd_file.put_line(fnd_file.log,
                        'Batch Name: ' || p_batch_name ||
                        ' PA Import Error / Deleting Data From Interface');
      v_pa_import_request_id := -1;
      v_pa_import_phase      := 'SUBMIT ERROR';
      v_pa_import_status     := 'SUBMIT ERROR';
      v_pa_import_message    := 'SUBMIT ERROR';
      -- Delete the Data from Interface and mark all records as ERROR for that Batch.
      DELETE FROM pa_transaction_interface_all
       WHERE transaction_source = p_transaction_source
         AND batch_name = p_batch_name;
      COMMIT;
    END IF;
    IF (UPPER(NVL(v_pa_import_phase, 'XX')) <> 'COMPLETE' OR
       UPPER(NVL(v_pa_import_status, 'XX')) <> 'NORMAL') THEN
      BEGIN
        UPDATE gets_pa_transaction_intf_all
           SET status           = 'ERROR',
               status_message   = 'PRC: Transaction Import Failure',
               last_update_date = SYSDATE,
               last_updated_by  = fnd_global.user_id
         WHERE transaction_source = p_transaction_source
           AND batch_name = p_batch_name;
      EXCEPTION
        WHEN OTHERS THEN
          errbuf  := SUBSTR('Error updating gets_pa_transaction_intf_all for ERROR2, error=>' ||
                            SQLERRM,
                            1,
                            255);
          retcode := 2;
          ROLLBACK;
          RETURN;
      END;
    END IF;
    -- Update Data in custom table with actual error message
    IF NVL(v_pa_import_request_id, -1) <> -1 THEN
      BEGIN
        SELECT COUNT(1)
          INTO v_error_count
          FROM pa_transaction_interface_all
         WHERE transaction_source = p_transaction_source
           AND batch_name = p_batch_name
           AND transaction_status_code = 'R';
      EXCEPTION
        WHEN OTHERS THEN
          v_error_count := 0;
      END;
      IF v_error_count = 0 THEN
        BEGIN
          UPDATE gets_pa_transaction_intf_all
             SET status                  = 'CREATED',
                 status_message          = NULL,
                 transaction_status_code = 'S',
                 last_update_date        = SYSDATE,
                 last_updated_by         = fnd_global.user_id
           WHERE transaction_source = p_transaction_source
             AND batch_name = p_batch_name;
        EXCEPTION
          WHEN OTHERS THEN
            errbuf  := SUBSTR('Error updating gets_pa_transaction_intf_all for CREATED1, error=>' ||
                              SQLERRM,
                              1,
                              255);
            retcode := 2;
            ROLLBACK;
            RETURN;
        END;
        FOR c2_rec IN c2_cur LOOP
          BEGIN
            UPDATE gets_pa_transaction_intf_all
               SET expenditure_item_id = c2_rec.expenditure_item_id,
                   expenditure_id      = c2_rec.expenditure_id,
                   last_update_date    = SYSDATE,
                   last_updated_by     = fnd_global.user_id
             WHERE org_id = c2_rec.org_id
               AND project_id = c2_rec.project_id
               AND task_id = c2_rec.task_id
               AND transaction_source = c2_rec.transaction_source
               AND batch_name = c2_rec.attribute9
               AND expenditure_type = c2_rec.expenditure_type
               AND orig_transaction_reference =
                   c2_rec.orig_transaction_reference;
          EXCEPTION
            WHEN OTHERS THEN
              errbuf  := SUBSTR('Error updating gets_pa_transaction_intf_all for CREATED2, error=>' ||
                                SQLERRM,
                                1,
                                255);
              retcode := 2;
              ROLLBACK;
              RETURN;
          END;
          IF c2_rec.transaction_source = 'Warranty Provision' THEN
            fnd_file.put_line(fnd_file.LOG,
                              'Warranty Reserve actual amount for Project: ' ||
                              c2_rec.attribute1);
            fnd_file.put_line(fnd_file.LOG,
                              'Warranty Reserve Actual => [((Forecast Warranty Reserve/Forecast Sales)*Actual Sales)-Accumulated Warranty Reserve]' ||
                              ' = ' || c2_rec.expenditure_comment || ' = ' ||
                              c2_rec.attribute10);
          ELSE
            fnd_file.put_line(fnd_file.LOG,
                              'Warranty Cost total amount for Project: ' ||
                              c2_rec.attribute1 || 'and Task: ' ||
                              c2_rec.attribute1 || ' and for Period: ' ||
                              c2_rec.attribute8 || ' => ' ||
                              c2_rec.raw_cost);
          END IF;
        END LOOP;
      ELSE
        FOR c1_rec IN c1_cur LOOP
          v_transaction_rejection_reason := get_rejection_reason(c1_rec.transaction_rejection_code);
          BEGIN
            UPDATE gets_pa_transaction_intf_all
               SET status                     = 'ERROR',
                   transaction_status_code    = c1_rec.transaction_status_code,
                   transaction_rejection_code = c1_rec.transaction_rejection_code,
                   status_message             = v_transaction_rejection_reason,
                   last_update_date           = SYSDATE,
                   last_updated_by            = fnd_global.user_id
             WHERE org_id = c1_rec.org_id
               AND project_id = c1_rec.project_id
               AND task_id = c1_rec.task_id
               AND transaction_source = c1_rec.transaction_source
               AND batch_name = c1_rec.batch_name
               AND expenditure_type = c1_rec.expenditure_type
               AND orig_transaction_reference =
                   c1_rec.orig_transaction_reference;
          EXCEPTION
            WHEN OTHERS THEN
              errbuf  := SUBSTR('Error updating gets_pa_transaction_intf_all for ERROR4, error=>' ||
                                SQLERRM,
                                1,
                                255);
              retcode := 2;
              ROLLBACK;
              RETURN;
          END;
          fnd_file.put_line(fnd_file.LOG,
                            'PRC Transaction Import Failure for Project#' ||
                            c1_rec.project_number || ' => ' ||
                            c1_rec.transaction_rejection_code || '-' ||
                            v_transaction_rejection_reason);
        END LOOP;
        DELETE FROM pa_transaction_interface_all
         WHERE transaction_source = p_transaction_source
           AND batch_name = p_batch_name;
        COMMIT;
      END IF;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := SUBSTR('Unexpected Error in RUN_PA_IMPORT procedure, error=>' ||
                        SQLERRM,
                        1,
                        255);
      retcode := 2;
      ROLLBACK;
      RETURN;
  END run_pa_import;
  -----------------------------------------------------------------------------------------------------------------------
  -- purpose : This procedure will be used to print debug messages for debugging purpose.
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE print_debug_message(p_msg_text IN VARCHAR2) IS
  BEGIN
    IF NVL(g_debug_level, 1) > 5 THEN
      dbms_output.put_line(p_msg_text);
      fnd_file.put_line(fnd_file.log, p_msg_text);
    END IF;
  END print_debug_message;
  -----------------------------------------------------------------------------------------------------------------------
  -- Function to get PA transaction rejection reason
  -----------------------------------------------------------------------------------------------------------------------
  FUNCTION get_rejection_reason(p_transaction_rejection_code gets_pa_transaction_intf_all.transaction_rejection_code%TYPE)
    RETURN VARCHAR2 IS
    v_transaction_rejection_reason VARCHAR2(2000);
  BEGIN
    SELECT meaning || ': ' || description
      INTO v_transaction_rejection_reason
      FROM pa_lookups
     WHERE lookup_type = 'TRANSACTION REJECTION REASON'
       AND lookup_code = p_transaction_rejection_code
       AND enabled_flag = 'Y'
       AND trunc(SYSDATE) BETWEEN
           nvl(trunc(start_date_active), trunc(SYSDATE)) AND
           nvl(trunc(end_date_active), trunc(SYSDATE));
    RETURN v_transaction_rejection_reason;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN NULL;
  END get_rejection_reason;
  -----------------------------------------------------------------------------------------------------------------------
  -- Procedure to calculate Warranty cost
  -----------------------------------------------------------------------------------------------------------------------
  PROCEDURE warranty_cost(errbuf               OUT VARCHAR2,
                          retcode              OUT NUMBER,
                          p_project_num        IN pa_projects_all.segment1%TYPE,
                          p_project_type       IN pa_projects_all.project_type%TYPE,
                          p_ou_name            IN gets_inv_op_unit_info.short_name%TYPE,
                          p_task_number        IN pa_tasks.task_number%TYPE DEFAULT NULL,
                          p_transaction_source IN VARCHAR2,
                          p_exp_type           IN VARCHAR2) AS
  
    CURSOR c1_cur IS
      SELECT pa.segment1,
             pa.project_id,
             ppt.project_type,
             pa.org_id,
             hou.short_name pa_org
        FROM pa_projects_all       pa,
             pa_project_types_all  ppt,
             gets_inv_op_unit_info hou
       WHERE pa.project_type = ppt.project_type
         AND pa.org_id = ppt.org_id
         and pa.org_id = hou.org_id
         AND pa.template_flag = 'N'
         AND pa.project_status_code = 'APPROVED'
         AND pa.project_type = nvl(p_project_type, pa.project_type)
         AND pa.project_type IN
             (SELECT meaning
                FROM fnd_lookup_values_vl
               WHERE lookup_type = 'GETS_PA_WARRANTY_CST_TYPES'
                 AND REGEXP_SUBSTR(lookup_code, '[^_]+', 1, 1) =
                     hou.short_name
                 AND enabled_flag = 'Y'
                 AND trunc(SYSDATE) BETWEEN
                     nvl(start_date_active, trunc(SYSDATE)) AND
                     nvl(end_date_active, trunc(SYSDATE)))
         AND pa.segment1 = nvl(p_project_num, pa.segment1)
         AND hou.short_name = nvl(p_ou_name, hou.short_name)
       ORDER BY pa.project_id;
  
    v_pa_custom_intf_rec         gets_pa_transaction_intf_all%ROWTYPE;
    v_orig_transaction_reference gets_pa_transaction_intf_all.orig_transaction_reference%TYPE := NULL;
    v_cost_total                 NUMBER := 0;
    v_cost_total_not_wcost       NUMBER := 0;
    v_cost_total_wcost           NUMBER := 0;
    v_error_flag                 VARCHAR2(1);
    v_task_id                    pa_tasks.task_id%TYPE;
    v_task_num                   pa_tasks.task_number%TYPE;
    v_task_name                  pa_tasks.task_name%TYPE;
    v_error_message              VARCHAR2(2000);
    v_exp_date                   DATE := SYSDATE;
  
  BEGIN
    errbuf       := 'Sucessfully Completed';
    retcode      := 0;
    v_error_flag := 'N';
    dbms_output.put_line('WARRANTY_COST start=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'WARRANTY_COST start=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    FOR c1_rec IN c1_cur LOOP
      v_cost_total_not_wcost := 0;
      v_cost_total_wcost     := 0;
      v_cost_total           := 0;
      BEGIN
        SELECT pt.task_id, pt.task_number, pt.task_name
          INTO v_task_id, v_task_num, v_task_name
          FROM pa_tasks pt
         WHERE pt.project_id = c1_rec.project_id
           AND pt.task_number = nvl(p_task_number, g_task_number);
      EXCEPTION
        WHEN OTHERS THEN
          v_task_id    := NULL;
          v_task_num   := NULL;
          v_task_name  := NULL;
          v_error_flag := 'Y';
      END;
      IF v_error_flag <> 'Y' THEN
        BEGIN
          BEGIN
            SELECT NVL(SUM(NVL(pcdla.amount, 0)), 0)
              INTO v_cost_total_not_wcost
              FROM pa_expenditure_items_all       peia,
                   pa_cost_distribution_lines_all pcdla
             WHERE peia.expenditure_item_id = pcdla.expenditure_item_id
               AND peia.project_id = c1_rec.project_id
               AND pcdla.transfer_status_code IN ('V', 'A')
               AND transaction_source <> p_transaction_source;
          EXCEPTION
            WHEN OTHERS THEN
              v_error_flag    := 'Y';
              v_error_message := 'Error calculating total cost for Project: ' ||
                                 c1_rec.segment1 || '; ' || SQLERRM;
          END;
          BEGIN
            SELECT NVL(SUM(NVL(pcdla.amount, 0)), 0)
              INTO v_cost_total_wcost
              FROM pa_expenditure_items_all       peia,
                   pa_cost_distribution_lines_all pcdla
             WHERE peia.expenditure_item_id = pcdla.expenditure_item_id
               AND peia.project_id = c1_rec.project_id
               AND transaction_source = p_transaction_source
               AND expenditure_type <>
                   REGEXP_SUBSTR(p_exp_type, '[^|]+', 1, 1);
          EXCEPTION
            WHEN OTHERS THEN
              v_error_flag    := 'Y';
              v_error_message := 'Error calculating total cost for Project: ' ||
                                 c1_rec.segment1 ||
                                 ' and Transaction Source: ' ||
                                 p_transaction_source || '; ' || SQLERRM;
          END;
          v_cost_total := v_cost_total_not_wcost + v_cost_total_wcost;
          IF v_error_flag <> 'Y' THEN
            IF v_cost_total <> 0 THEN
              BEGIN
                v_pa_custom_intf_rec.transaction_source    := p_transaction_source;
                v_pa_custom_intf_rec.batch_name            := p_transaction_source || '_' ||
                                                              to_char(SYSDATE,
                                                                      'YYYY-MM-DD HH:MM:SS');
                v_pa_custom_intf_rec.organization_name     := get_project_org(c1_rec.project_id);
                v_pa_custom_intf_rec.expenditure_item_date := v_exp_date;
                IF trim(to_char(v_exp_date, 'DAY')) = 'SUNDAY' THEN
                  v_pa_custom_intf_rec.expenditure_ending_date := v_exp_date;
                ELSE
                  v_pa_custom_intf_rec.expenditure_ending_date := next_day(v_exp_date,
                                                                           'SUNDAY');
                END IF;
                print_debug_message('1');
                v_pa_custom_intf_rec.project_number              := c1_rec.segment1;
                v_pa_custom_intf_rec.expenditure_type            := REGEXP_SUBSTR(p_exp_type,
                                                                                  '[^|]+',
                                                                                  1,
                                                                                  1);
                v_pa_custom_intf_rec.quantity                    := 1;
                v_pa_custom_intf_rec.raw_cost                    := v_cost_Total;
                v_pa_custom_intf_rec.burdened_cost               := v_cost_Total;
                v_pa_custom_intf_rec.transaction_status_code     := 'P';
                v_pa_custom_intf_rec.project_id                  := c1_rec.project_id;
                v_pa_custom_intf_rec.task_number                 := v_task_num;
                v_pa_custom_intf_rec.task_id                     := v_task_id;
                v_pa_custom_intf_rec.system_linkage              := NULL;
                v_pa_custom_intf_rec.unmatched_negative_txn_flag := 'Y';
                v_pa_custom_intf_rec.org_id                      := c1_rec.org_id;
                v_pa_custom_intf_rec.created_by                  := fnd_global.user_id;
                v_pa_custom_intf_rec.creation_date               := SYSDATE;
                v_pa_custom_intf_rec.last_updated_by             := fnd_global.user_id;
                v_pa_custom_intf_rec.last_update_date            := SYSDATE;
                print_debug_message('1.1');
                v_pa_custom_intf_rec.expenditure_comment         := NULL;
                v_pa_custom_intf_rec.interface_id                := NULL;
                v_pa_custom_intf_rec.user_transaction_source     := NULL;
                v_pa_custom_intf_rec.employee_number             := NULL;
                v_pa_custom_intf_rec.non_labor_resource          := NULL;
                v_pa_custom_intf_rec.non_labor_resource_org_name := NULL;
                v_pa_custom_intf_rec.transaction_rejection_code  := NULL;
                v_pa_custom_intf_rec.expenditure_id              := NULL;
                v_pa_custom_intf_rec.attribute_category          := NULL;
                v_pa_custom_intf_rec.attribute1                  := c1_rec.segment1;
                v_pa_custom_intf_rec.attribute2                  := c1_rec.project_type;
                v_pa_custom_intf_rec.attribute3                  := NULL;
                v_pa_custom_intf_rec.attribute4                  := NULL;
                v_pa_custom_intf_rec.attribute5                  := NULL;
                v_pa_custom_intf_rec.attribute6                  := NULL;
                v_pa_custom_intf_rec.attribute7                  := NULL;
                v_pa_custom_intf_rec.attribute8                  := to_char(SYSDATE,
                                                                            'MON-YY');
                print_debug_message('1.2');
                v_pa_custom_intf_rec.attribute9 := v_pa_custom_intf_rec.batch_name;
                print_debug_message('1.3');
                v_pa_custom_intf_rec.attribute10 := v_cost_Total;
                print_debug_message('1.4');
                v_pa_custom_intf_rec.raw_cost_rate                 := NULL;
                v_pa_custom_intf_rec.expenditure_item_id           := NULL;
                v_pa_custom_intf_rec.dr_code_combination_id        := NULL;
                v_pa_custom_intf_rec.cr_code_combination_id        := NULL;
                v_pa_custom_intf_rec.cdl_system_reference1         := NULL;
                v_pa_custom_intf_rec.cdl_system_reference2         := NULL;
                v_pa_custom_intf_rec.cdl_system_reference3         := NULL;
                v_pa_custom_intf_rec.gl_date                       := NULL;
                v_pa_custom_intf_rec.burdened_cost_rate            := NULL;
                v_pa_custom_intf_rec.receipt_currency_amount       := NULL;
                v_pa_custom_intf_rec.receipt_currency_code         := NULL;
                v_pa_custom_intf_rec.receipt_exchange_rate         := NULL;
                v_pa_custom_intf_rec.denom_currency_code           := NULL;
                v_pa_custom_intf_rec.denom_raw_cost                := v_cost_Total;
                v_pa_custom_intf_rec.denom_burdened_cost           := NULL;
                v_pa_custom_intf_rec.acct_rate_date                := NULL;
                v_pa_custom_intf_rec.acct_rate_type                := NULL;
                v_pa_custom_intf_rec.acct_exchange_rate            := NULL;
                v_pa_custom_intf_rec.acct_raw_cost                 := NULL;
                v_pa_custom_intf_rec.acct_burdened_cost            := NULL;
                v_pa_custom_intf_rec.acct_exchange_rounding_limit  := NULL;
                v_pa_custom_intf_rec.project_currency_code         := NULL;
                v_pa_custom_intf_rec.project_rate_date             := NULL;
                v_pa_custom_intf_rec.project_rate_type             := NULL;
                v_pa_custom_intf_rec.project_exchange_rate         := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference1       := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference2       := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference3       := NULL;
                v_pa_custom_intf_rec.orig_user_exp_txn_reference   := NULL;
                v_pa_custom_intf_rec.override_to_organization_name := NULL;
                v_pa_custom_intf_rec.reversed_orig_txn_reference   := NULL;
                v_pa_custom_intf_rec.billable_flag                 := NULL;
                v_pa_custom_intf_rec.person_business_group_name    := NULL;
                v_pa_custom_intf_rec.projfunc_currency_code        := NULL;
                v_pa_custom_intf_rec.projfunc_cost_rate_type       := NULL;
                v_pa_custom_intf_rec.projfunc_cost_rate_date       := NULL;
                v_pa_custom_intf_rec.projfunc_cost_exchange_rate   := NULL;
                v_pa_custom_intf_rec.project_raw_cost              := NULL;
                v_pa_custom_intf_rec.project_burdened_cost         := NULL;
                v_pa_custom_intf_rec.assignment_name               := NULL;
                v_pa_custom_intf_rec.work_type_name                := NULL;
                v_pa_custom_intf_rec.cdl_system_reference4         := NULL;
                v_pa_custom_intf_rec.accrual_flag                  := NULL;
                v_pa_custom_intf_rec.person_id                     := NULL;
                v_pa_custom_intf_rec.organization_id               := NULL;
                v_pa_custom_intf_rec.non_labor_resource_org_id     := NULL;
                v_pa_custom_intf_rec.override_to_organization_id   := NULL;
                v_pa_custom_intf_rec.assignment_id                 := NULL;
                v_pa_custom_intf_rec.work_type_id                  := NULL;
                v_pa_custom_intf_rec.person_business_group_id      := NULL;
                v_pa_custom_intf_rec.inventory_item_id             := NULL;
                v_pa_custom_intf_rec.wip_resource_id               := NULL;
                v_pa_custom_intf_rec.unit_of_measure               := NULL;
                v_pa_custom_intf_rec.po_number                     := NULL;
                v_pa_custom_intf_rec.po_header_id                  := NULL;
                v_pa_custom_intf_rec.po_line_num                   := NULL;
                v_pa_custom_intf_rec.po_line_id                    := NULL;
                v_pa_custom_intf_rec.person_type                   := NULL;
                v_pa_custom_intf_rec.po_price_type                 := NULL;
                v_pa_custom_intf_rec.vendor_id                     := NULL;
                v_pa_custom_intf_rec.vendor_number                 := NULL;
                v_pa_custom_intf_rec.status                        := 'ELIGIBLE';
                v_pa_custom_intf_rec.status_message                := NULL;
                print_debug_message('1.5');
                v_orig_transaction_reference := get_orig_transaction_reference(c1_rec.org_id,
                                                                               c1_rec.project_id,
                                                                               v_task_id,
                                                                               REGEXP_SUBSTR(p_exp_type,
                                                                                             '[^|]+',
                                                                                             1,
                                                                                             1),
                                                                               p_transaction_source);
                print_debug_message('2');
                IF v_orig_transaction_reference IS NULL THEN
                  print_debug_message('3');
                  v_pa_custom_intf_rec.orig_transaction_reference := gets_pa_orig_trans_ref_s.NEXTVAL;
                  BEGIN
                    gets_pa_interface_ins(errbuf,
                                          retcode,
                                          v_pa_custom_intf_rec);
                  EXCEPTION
                    WHEN OTHERS THEN
                      errbuf  := SUBSTR('Error calling gets_pa_interface_ins, error=>' ||
                                        SQLERRM,
                                        1,
                                        255);
                      retcode := 2;
                      ROLLBACK;
                      RETURN;
                  END;
                ELSE
                  print_debug_message('4');
                  v_pa_custom_intf_rec.orig_transaction_reference := v_orig_transaction_reference;
                  BEGIN
                    gets_pa_interface_upd(errbuf,
                                          retcode,
                                          v_pa_custom_intf_rec);
                  EXCEPTION
                    WHEN OTHERS THEN
                      errbuf  := SUBSTR('Error calling gets_pa_interface_upd, error=>' ||
                                        SQLERRM,
                                        1,
                                        255);
                      retcode := 2;
                      ROLLBACK;
                      RETURN;
                  END;
                END IF;
                print_debug_message('5');
                v_pa_custom_intf_rec.transaction_source    := p_transaction_source;
                v_pa_custom_intf_rec.batch_name            := v_pa_custom_intf_rec.batch_name;
                v_pa_custom_intf_rec.organization_name     := get_project_org(c1_rec.project_id);
                v_pa_custom_intf_rec.expenditure_item_date := v_exp_date;
                IF trim(to_char(v_exp_date, 'DAY')) = 'SUNDAY' THEN
                  v_pa_custom_intf_rec.expenditure_ending_date := v_exp_date;
                ELSE
                  v_pa_custom_intf_rec.expenditure_ending_date := next_day(v_exp_date,
                                                                           'SUNDAY');
                END IF;
                print_debug_message('6');
                v_pa_custom_intf_rec.project_number              := c1_rec.segment1;
                v_pa_custom_intf_rec.expenditure_type            := REGEXP_SUBSTR(p_exp_type,
                                                                                  '[^|]+',
                                                                                  1,
                                                                                  2);
                v_pa_custom_intf_rec.quantity                    := 1;
                v_pa_custom_intf_rec.raw_cost                    := -v_cost_Total;
                v_pa_custom_intf_rec.burdened_cost               := -v_cost_Total;
                v_pa_custom_intf_rec.transaction_status_code     := 'P';
                v_pa_custom_intf_rec.project_id                  := c1_rec.project_id;
                v_pa_custom_intf_rec.task_number                 := v_task_num;
                v_pa_custom_intf_rec.task_id                     := v_task_id;
                v_pa_custom_intf_rec.system_linkage              := NULL;
                v_pa_custom_intf_rec.unmatched_negative_txn_flag := 'Y';
                v_pa_custom_intf_rec.org_id                      := c1_rec.org_id;
                v_pa_custom_intf_rec.created_by                  := fnd_global.user_id;
                v_pa_custom_intf_rec.creation_date               := SYSDATE;
                v_pa_custom_intf_rec.last_updated_by             := fnd_global.user_id;
                v_pa_custom_intf_rec.last_update_date            := SYSDATE;
                print_debug_message('6.1');
                v_pa_custom_intf_rec.expenditure_comment         := NULL;
                v_pa_custom_intf_rec.interface_id                := NULL;
                v_pa_custom_intf_rec.user_transaction_source     := NULL;
                v_pa_custom_intf_rec.employee_number             := NULL;
                v_pa_custom_intf_rec.non_labor_resource          := NULL;
                v_pa_custom_intf_rec.non_labor_resource_org_name := NULL;
                v_pa_custom_intf_rec.transaction_rejection_code  := NULL;
                v_pa_custom_intf_rec.expenditure_id              := NULL;
                v_pa_custom_intf_rec.attribute_category          := NULL;
                v_pa_custom_intf_rec.attribute1                  := c1_rec.segment1;
                v_pa_custom_intf_rec.attribute2                  := c1_rec.project_type;
                v_pa_custom_intf_rec.attribute3                  := NULL;
                v_pa_custom_intf_rec.attribute4                  := NULL;
                v_pa_custom_intf_rec.attribute5                  := NULL;
                v_pa_custom_intf_rec.attribute6                  := NULL;
                v_pa_custom_intf_rec.attribute7                  := NULL;
                v_pa_custom_intf_rec.attribute8                  := to_char(SYSDATE,
                                                                            'MON-YY');
                print_debug_message('6.2');
                v_pa_custom_intf_rec.attribute9 := v_pa_custom_intf_rec.batch_name;
                print_debug_message('6.3');
                v_pa_custom_intf_rec.attribute10 := -v_cost_Total;
                print_debug_message('6.4');
                v_pa_custom_intf_rec.raw_cost_rate                 := NULL;
                v_pa_custom_intf_rec.expenditure_item_id           := NULL;
                v_pa_custom_intf_rec.dr_code_combination_id        := NULL;
                v_pa_custom_intf_rec.cr_code_combination_id        := NULL;
                v_pa_custom_intf_rec.cdl_system_reference1         := NULL;
                v_pa_custom_intf_rec.cdl_system_reference2         := NULL;
                v_pa_custom_intf_rec.cdl_system_reference3         := NULL;
                v_pa_custom_intf_rec.gl_date                       := NULL;
                v_pa_custom_intf_rec.burdened_cost_rate            := NULL;
                v_pa_custom_intf_rec.receipt_currency_amount       := NULL;
                v_pa_custom_intf_rec.receipt_currency_code         := NULL;
                v_pa_custom_intf_rec.receipt_exchange_rate         := NULL;
                v_pa_custom_intf_rec.denom_currency_code           := NULL;
                v_pa_custom_intf_rec.denom_raw_cost                := -v_cost_Total;
                v_pa_custom_intf_rec.denom_burdened_cost           := NULL;
                v_pa_custom_intf_rec.acct_rate_date                := NULL;
                v_pa_custom_intf_rec.acct_rate_type                := NULL;
                v_pa_custom_intf_rec.acct_exchange_rate            := NULL;
                v_pa_custom_intf_rec.acct_raw_cost                 := NULL;
                v_pa_custom_intf_rec.acct_burdened_cost            := NULL;
                v_pa_custom_intf_rec.acct_exchange_rounding_limit  := NULL;
                v_pa_custom_intf_rec.project_currency_code         := NULL;
                v_pa_custom_intf_rec.project_rate_date             := NULL;
                v_pa_custom_intf_rec.project_rate_type             := NULL;
                v_pa_custom_intf_rec.project_exchange_rate         := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference1       := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference2       := NULL;
                v_pa_custom_intf_rec.orig_exp_txn_reference3       := NULL;
                v_pa_custom_intf_rec.orig_user_exp_txn_reference   := NULL;
                v_pa_custom_intf_rec.override_to_organization_name := NULL;
                v_pa_custom_intf_rec.reversed_orig_txn_reference   := NULL;
                v_pa_custom_intf_rec.billable_flag                 := NULL;
                v_pa_custom_intf_rec.person_business_group_name    := NULL;
                v_pa_custom_intf_rec.projfunc_currency_code        := NULL;
                v_pa_custom_intf_rec.projfunc_cost_rate_type       := NULL;
                v_pa_custom_intf_rec.projfunc_cost_rate_date       := NULL;
                v_pa_custom_intf_rec.projfunc_cost_exchange_rate   := NULL;
                v_pa_custom_intf_rec.project_raw_cost              := NULL;
                v_pa_custom_intf_rec.project_burdened_cost         := NULL;
                v_pa_custom_intf_rec.assignment_name               := NULL;
                v_pa_custom_intf_rec.work_type_name                := NULL;
                v_pa_custom_intf_rec.cdl_system_reference4         := NULL;
                v_pa_custom_intf_rec.accrual_flag                  := NULL;
                v_pa_custom_intf_rec.person_id                     := NULL;
                v_pa_custom_intf_rec.organization_id               := NULL;
                v_pa_custom_intf_rec.non_labor_resource_org_id     := NULL;
                v_pa_custom_intf_rec.override_to_organization_id   := NULL;
                v_pa_custom_intf_rec.assignment_id                 := NULL;
                v_pa_custom_intf_rec.work_type_id                  := NULL;
                v_pa_custom_intf_rec.person_business_group_id      := NULL;
                v_pa_custom_intf_rec.inventory_item_id             := NULL;
                v_pa_custom_intf_rec.wip_resource_id               := NULL;
                v_pa_custom_intf_rec.unit_of_measure               := NULL;
                v_pa_custom_intf_rec.po_number                     := NULL;
                v_pa_custom_intf_rec.po_header_id                  := NULL;
                v_pa_custom_intf_rec.po_line_num                   := NULL;
                v_pa_custom_intf_rec.po_line_id                    := NULL;
                v_pa_custom_intf_rec.person_type                   := NULL;
                v_pa_custom_intf_rec.po_price_type                 := NULL;
                v_pa_custom_intf_rec.vendor_id                     := NULL;
                v_pa_custom_intf_rec.vendor_number                 := NULL;
                v_pa_custom_intf_rec.status                        := 'ELIGIBLE';
                v_pa_custom_intf_rec.status_message                := NULL;
                print_debug_message('6.5');
                v_orig_transaction_reference := get_orig_transaction_reference(c1_rec.org_id,
                                                                               c1_rec.project_id,
                                                                               v_task_id,
                                                                               REGEXP_SUBSTR(p_exp_type,
                                                                                             '[^|]+',
                                                                                             1,
                                                                                             2),
                                                                               p_transaction_source);
                print_debug_message('7');
                IF v_orig_transaction_reference IS NULL THEN
                  print_debug_message('8');
                  v_pa_custom_intf_rec.orig_transaction_reference := gets_pa_orig_trans_ref_s.NEXTVAL;
                  BEGIN
                    gets_pa_interface_ins(errbuf,
                                          retcode,
                                          v_pa_custom_intf_rec);
                  EXCEPTION
                    WHEN OTHERS THEN
                      errbuf  := SUBSTR('Error calling gets_pa_interface_ins, error=>' ||
                                        SQLERRM,
                                        1,
                                        255);
                      retcode := 2;
                      ROLLBACK;
                      RETURN;
                  END;
                ELSE
                  print_debug_message('9');
                  v_pa_custom_intf_rec.orig_transaction_reference := v_orig_transaction_reference;
                  BEGIN
                    gets_pa_interface_upd(errbuf,
                                          retcode,
                                          v_pa_custom_intf_rec);
                  EXCEPTION
                    WHEN OTHERS THEN
                      errbuf  := SUBSTR('Error calling gets_pa_interface_upd, error=>' ||
                                        SQLERRM,
                                        1,
                                        255);
                      retcode := 2;
                      ROLLBACK;
                      RETURN;
                  END;
                END IF;
                print_debug_message('10');
              EXCEPTION
                WHEN OTHERS THEN
                  errbuf  := SUBSTR('Error assigning data to v_pa_custom_intf_rec, error=>' ||
                                    SQLERRM,
                                    1,
                                    255);
                  retcode := 2;
                  ROLLBACK;
                  RETURN;
              END;
            ELSE
              fnd_file.put_line(fnd_file.LOG,
                                ' Zero Cost in total cost actuals of the Project: ' ||
                                c1_rec.segment1);
            END IF;
          ELSE
            fnd_file.put_line(fnd_file.LOG, v_error_message);
          END IF;
        EXCEPTION
          WHEN OTHERS THEN
            fnd_file.put_line(fnd_file.LOG, 'Error: ' || SQLERRM);
        END;
      ELSE
        fnd_file.put_line(fnd_file.LOG,
                          g_task_number ||
                          ' task not present for Project: ' ||
                          c1_rec.segment1);
      END IF;
    END LOOP;
    COMMIT;
    dbms_output.put_line('WARRANTY_COST completed=>' ||
                         TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
    fnd_file.put_line(fnd_file.LOG,
                      'WARRANTY_COST completed=>' ||
                      TO_CHAR(SYSDATE, 'DD-MON-RR HH24:MI:SS'));
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := SUBSTR('Unexpected Error in WARRANTY_COST procedure, error=>' ||
                        SQLERRM,
                        1,
                        255);
      retcode := 2;
      ROLLBACK;
      RETURN;
  END warranty_cost;
  -----------------------------------------------------------------------------------------------------------------------
  -- Functions for Warranty Report
  -----------------------------------------------------------------------------------------------------------------------

  FUNCTION get_po_num(f_po_header_id NUMBER, f_source VARCHAR2)
    RETURN VARCHAR2 IS
    v_po_num VARCHAR2(100);
  BEGIN
    IF f_source IN ('PO RECEIPT',
                    'PO RECEIPT PRICE ADJ',
                    'PO RECEIPT NRTAX',
                    'PO RECEIPT NRTAX PRICE ADJ',
                    'CSE_PO_RECEIPT',
                    'CSE_PO_RECEIPT_DEPR',
                    'CSE_INV_PO',
                    'CSE_INV_WITH_ACCOUNTS',
                    'CSE_INV_WITHOUT_ACCOUNTS') THEN
      SELECT poh.segment1
        INTO v_po_num
        FROM po_headers_all poh
       WHERE po_header_id = f_po_header_id;
    ELSE
      v_po_num := NULL;
    END IF;
    RETURN v_po_num;
  EXCEPTION
    WHEN OTHERS THEN
      v_po_num := NULL;
      RETURN v_po_num;
  END get_po_num;

  FUNCTION get_proj_class_code(f_Class_category VARCHAR2,
                               f_project_id     NUMBER
                               
                               ) RETURN VARCHAR2 IS
    v_pa_class_code VARCHAR2(100);
  BEGIN
    select class_code
      INTO v_pa_class_code
      from PA_PROJECT_CLASSES
     where class_category = f_Class_category
       and project_id = f_project_id;
  
    RETURN v_pa_class_code;
  EXCEPTION
    WHEN OTHERS THEN
      v_pa_class_code := NULL;
      RETURN v_pa_class_code;
  END get_proj_class_code;

  FUNCTION get_job_name(f_job_id NUMBER
                        
                        ) RETURN VARCHAR2 IS
    v_job_name VARCHAR2(100);
  BEGIN
    select name INTO v_job_name from per_jobs where job_id = f_job_id;
  
    RETURN v_job_name;
  EXCEPTION
    WHEN OTHERS THEN
      v_job_name := NULL;
      RETURN v_job_name;
  END get_job_name;

  FUNCTION get_cost_dist_error(f_cost_error VARCHAR2
                               
                               ) RETURN VARCHAR2 IS
    v_dist_error VARCHAR2(240);
  BEGIN
    SELECT flv.description
      into v_dist_error
      FROM fnd_lookup_values flv
     WHERE flv.LOOKUP_TYPE = 'COST DIST REJECTION CODE'
       and flv.lookup_code = f_cost_error
       AND flv.LANGUAGE = 'US'
       AND flv.ENABLED_FLAG = 'Y'
       and TRUNC(SYSDATE) BETWEEN
           TRUNC(NVL(flv.START_DATE_active, SYSDATE)) AND
           TRUNC(NVL(flv.end_date_active, SYSDATE));
  
    RETURN v_dist_error;
  EXCEPTION
    WHEN OTHERS THEN
      v_dist_error := NULL;
      RETURN v_dist_error;
  END get_cost_dist_error;

  FUNCTION get_task_number(f_task_id NUMBER
                           
                           ) RETURN VARCHAR2 IS
    v_task_num VARCHAR2(100);
  BEGIN
    select task_number
      INTO v_task_num
      from PA_TASKS
     where task_id = f_task_id;
  
    RETURN v_task_num;
  EXCEPTION
    WHEN OTHERS THEN
      v_task_num := NULL;
      RETURN v_task_num;
  END get_task_number;

  FUNCTION get_inv_item(f_inv_id NUMBER,
                        f_org_id NUMBER
                        
                        ) RETURN VARCHAR2 IS
    v_inv_item VARCHAR2(100);
  BEGIN
    select segment1
      into v_inv_item
      from MTL_SYSTEM_ITEMS_B
     where inventory_item_id = f_inv_id
       and ORGANIZATION_ID = f_org_id;
  
    RETURN v_inv_item;
  EXCEPTION
    WHEN OTHERS THEN
      v_inv_item := NULL;
      RETURN v_inv_item;
  END get_inv_item; --- 6func

  FUNCTION get_wip_resource(f_res_id NUMBER
                            
                            ) RETURN VARCHAR2 IS
    v_res_code VARCHAR2(100);
  BEGIN
    SELECT resource_code
      into v_res_code
      from BOM_RESOURCES
     where resource_id = f_res_id;
  
    RETURN v_res_code;
  EXCEPTION
    WHEN OTHERS THEN
      v_res_code := NULL;
      RETURN v_res_code;
  END get_wip_resource; --- 7func  

  FUNCTION get_po_line_num(f_po_line_id NUMBER
                           
                           ) RETURN VARCHAR2 IS
    v_po_line_num VARCHAR2(100);
  BEGIN
    select line_num
      INTO v_po_line_num
      from po_lines_all
     where po_line_id = f_po_line_id;
  
    RETURN v_po_line_num;
  EXCEPTION
    WHEN OTHERS THEN
      v_po_line_num := NULL;
      RETURN v_po_line_num;
  END get_po_line_num;

  FUNCTION get_person(f_expd_id NUMBER
                      
                      ) RETURN VARCHAR2 IS
    v_person_name VARCHAR2(100);
  BEGIN
    select papf.full_name
      INTO v_person_name
      from per_all_people_f papf, pa_expenditures_all pea
     where pea.expenditure_id = f_expd_id
       AND papf.person_id = pea.incurred_by_person_id;
  
    RETURN v_person_name;
  EXCEPTION
    WHEN OTHERS THEN
      v_person_name := NULL;
      RETURN v_person_name;
  END get_person;

  FUNCTION get_ccid(f_ccid NUMBER
                    
                    ) RETURN VARCHAR2 IS
    v_acct VARCHAR2(100);
  BEGIN
    select gcc.segment1 || '-' || gcc.segment2 || '-' || gcc.segment3 || '-' ||
           gcc.segment4 || '-' || gcc.segment5 || '-' || gcc.segment6 || '-' ||
           gcc.segment7 || '-' || gcc.segment8 || '-' || gcc.segment9 || '-' ||
           gcc.segment10 || '-' || gcc.segment11
      INTO v_acct
      from gl_code_combinations gcc
     where gcc.code_combination_id = f_ccid;
  
    RETURN v_acct;
  EXCEPTION
    WHEN OTHERS THEN
      v_acct := NULL;
      RETURN v_acct;
  END get_ccid;

  ---------------------------------------------------------------------------------------------------                                                                                        |
  --Purpose: generate excel report to display.                                                                                         |
  ----------------------------------------------------------------------------------------------------
  PROCEDURE PRINT_XML_TAG(P_XML_TAG IN VARCHAR2, P_XML_DATA IN VARCHAR2) IS
  BEGIN
    GETS_GENERIC_UTILITIES_PKG.PRINT_XML_TAG(P_XML_TAG  => P_XML_TAG,
                                             P_XML_DATA => P_XML_DATA);
  END PRINT_XML_TAG;

  ----------------------------------------------------------------------------------------------------                                                                                        |
  --Purpose: generate excel report to display PA transactions.                                                                                         |
  ----------------------------------------------------------------------------------------------------
  PROCEDURE gets_pa_warranty_rpt_prc(errbuf                       OUT NOCOPY VARCHAR2,
                                     retcode                      OUT NOCOPY NUMBER,
                                     P_project_type               IN VARCHAR2,
                                     p_EXPENDITURE_ITEM_DATE_FROM IN VARCHAR2,
                                     p_EXPENDITURE_ITEM_DATE_TO   IN VARCHAR2,
                                     P_Project_Number             IN VARCHAR2 DEFAULT NULL,
                                     p_ou_name                    IN VARCHAR2) IS
  
    CURSOR c_data IS
      select EXPENDITURE_ITEM_ID,
             PROJECT_NUMBER,
             GL_PROJECT_SEGMENT,
             PARENT_PROJECT_NUMBER,
             PRODUCT_LINE,
             TASK_NUMBER,
             EXPENDITURE_ITEM_DATE,
             EXPENDITURE_TYPE,
             TRANSACTION_SOURCE,
             QUANTITY,
             JOB_NAME,
             RAW_COST_RATE,
             SUPPLIER_NAME,
             MISC_CREDIT_ACCOUNT,
             CURRENCY,
             COST_AMOUNT,
             ACCOUNTED_CURRENCY,
             ACCOUNTED_COST,
             PO_NUMBER,
             PO_LINE_NUMBER,
             WIP_RESOURCE,
             Item_Number,
             COST_DIST_REJECTION_CODE,
             COST_DISTRIBUTED_FLAG,
             GL_PERIOD_NAME,
             DEBIT_CODE_COMBINATION,
             CREDIT_CODE_COMBINATION,
             ORIG_TRANSACTION_REFERENCE
        from (Select peia.EXPENDITURE_ITEM_ID,
                     ppa.segment1 PROJECT_NUMBER,
                     ppa.attribute7 GL_PROJECT_SEGMENT,
                     ppa.attribute4 PARENT_PROJECT_NUMBER,
                     GETS_PA_WARRANTY_PKG.get_proj_class_code('Product Line',
                                                              peia.project_id) PRODUCT_LINE, -- FUNC
                     GETS_PA_WARRANTY_PKG.get_task_number(peia.task_id) TASK_NUMBER, --func
                     peia.EXPENDITURE_ITEM_DATE,
                     peia.EXPENDITURE_TYPE,
                     peia.TRANSACTION_SOURCE,
                     peia.QUANTITY,
                     GETS_PA_WARRANTY_PKG.get_job_name(peia.job_id) JOB_NAME, --func
                     peia.RAW_COST_RATE,
                     decode(peia.system_linkage_function,
                            'ST',
                            GETS_PA_WARRANTY_PKG.get_person(peia.expenditure_id),
                            'OT',
                            GETS_PA_WARRANTY_PKG.get_person(peia.expenditure_id),
                            'PJ',
                            GETS_PA_WARRANTY_PKG.get_person(peia.expenditure_id),
                            sup.vendor_name) SUPPLIER_NAME,
                     peia.attribute6 MISC_CREDIT_ACCOUNT,
                     pcdla.DENOM_CURRENCY_CODE CURRENCY,
                     pcdla.BURDENED_COST COST_AMOUNT,
                     peia.ACCT_CURRENCY_CODE ACCOUNTED_CURRENCY,
                     peia.ACCT_BURDENED_COST ACCOUNTED_COST,
                     GETS_PA_WARRANTY_PKG.get_po_num(pcdla.system_reference2,
                                                     peia.transaction_source) PO_NUMBER, --FUNC
                     GETS_PA_WARRANTY_PKG.get_po_line_num(peia.PO_LINE_ID) PO_LINE_NUMBER, --FUNC
                     GETS_PA_WARRANTY_PKG.get_wip_resource(peia.WIP_RESOURCE_ID) WIP_RESOURCE, --FUNC
                     GETS_PA_WARRANTY_PKG.get_inv_item(peia.INVENTORY_ITEM_ID,
                                                       peia.org_id) Item_Number, --FUNC
                     GETS_PA_WARRANTY_PKG.get_cost_dist_error(peia.COST_DIST_REJECTION_CODE) COST_DIST_REJECTION_CODE, --FUNC
                     peia.COST_DISTRIBUTED_FLAG,
                     PCDLA.GL_PERIOD_NAME,
                     GETS_PA_WARRANTY_PKG.get_ccid(pcdla.dr_code_combination_id) DEBIT_CODE_COMBINATION,
                     GETS_PA_WARRANTY_PKG.get_ccid(pcdla.cr_code_combination_id) CREDIT_CODE_COMBINATION,
                     peia.ORIG_TRANSACTION_REFERENCE
                from Pa_expenditure_items_all       peia,
                     Pa_cost_distribution_lines_all pcdla,
                     pa_projects_all                ppa,
                     gets_inv_op_unit_info          hou,
                     ap_suppliers                   sup
               where peia.EXPENDITURE_ITEM_ID = pcdla.EXPENDITURE_ITEM_ID
                 AND ppa.project_id = peia.project_id
                 AND ppa.org_id = hou.org_id
                 AND hou.short_name = nvl(p_ou_name, hou.short_name)
                 AND ppa.segment1 = nvl(p_project_number, ppa.segment1)
                 AND ppa.project_type =
                     nvl(p_project_type, ppa.project_type)
                 and peia.vendor_id = sup.vendor_id(+)
                 and trunc(peia.EXPENDITURE_ITEM_DATE) BETWEEN
                     trunc(to_date(p_EXPENDITURE_ITEM_DATE_FROM, 'DD-MON-YY')) AND
                     trunc(to_date(p_EXPENDITURE_ITEM_DATE_TO, 'DD-MON-YY'))
              
              Union
              
              Select peia.EXPENDITURE_ITEM_ID,
                     ppa.segment1 PROJECT_NUMBER,
                     ppa.attribute7 GL_PROJECT_SEGMENT,
                     ppa.attribute4 PARENT_PROJECT_NUMBER,
                     GETS_PA_WARRANTY_PKG.get_proj_class_code('Product Line',
                                                              peia.project_id) PRODUCT_LINE, -- FUNC
                     GETS_PA_WARRANTY_PKG.get_task_number(peia.task_id) TASK_NUMBER, --func
                     peia.EXPENDITURE_ITEM_DATE,
                     peia.EXPENDITURE_TYPE,
                     peia.TRANSACTION_SOURCE,
                     peia.QUANTITY,
                     GETS_PA_WARRANTY_PKG.get_job_name(peia.job_id) JOB_NAME, --func
                     peia.RAW_COST_RATE,
                     decode(peia.system_linkage_function,
                            'ST',
                            GETS_PA_WARRANTY_PKG.get_person(peia.expenditure_id),
                            'OT',
                            GETS_PA_WARRANTY_PKG.get_person(peia.expenditure_id),
                            'PJ',
                            GETS_PA_WARRANTY_PKG.get_person(peia.expenditure_id),
                            sup.vendor_name) SUPPLIER_NAME,
                     peia.attribute6 MISC_CREDIT_ACCOUNT,
                     pcdla.DENOM_CURRENCY_CODE CURRENCY,
                     pcdla.BURDENED_COST COST_AMOUNT,
                     peia.ACCT_CURRENCY_CODE ACCOUNTED_CURRENCY,
                     peia.ACCT_BURDENED_COST ACCOUNTED_COST,
                     GETS_PA_WARRANTY_PKG.get_po_num(pcdla.system_reference2,
                                                     peia.transaction_source) PO_NUMBER, --FUNC
                     GETS_PA_WARRANTY_PKG.get_po_line_num(peia.PO_LINE_ID) PO_LINE_NUMBER, --FUNC
                     GETS_PA_WARRANTY_PKG.get_wip_resource(peia.WIP_RESOURCE_ID) WIP_RESOURCE, --FUNC
                     GETS_PA_WARRANTY_PKG.get_inv_item(peia.INVENTORY_ITEM_ID,
                                                       peia.org_id) Item_Number, --FUNC
                     GETS_PA_WARRANTY_PKG.get_cost_dist_error(peia.COST_DIST_REJECTION_CODE) COST_DIST_REJECTION_CODE, --FUNC
                     peia.COST_DISTRIBUTED_FLAG,
                     PCDLA.GL_PERIOD_NAME,
                     GETS_PA_WARRANTY_PKG.get_ccid(pcdla.dr_code_combination_id) DEBIT_CODE_COMBINATION,
                     GETS_PA_WARRANTY_PKG.get_ccid(pcdla.cr_code_combination_id) CREDIT_CODE_COMBINATION,
                     peia.ORIG_TRANSACTION_REFERENCE
                from Pa_expenditure_items_all       peia,
                     Pa_cost_distribution_lines_all pcdla,
                     pa_projects_all                ppa,
                     gets_inv_op_unit_info          hou,
                     ap_suppliers                   sup
               where peia.EXPENDITURE_ITEM_ID = pcdla.EXPENDITURE_ITEM_ID
                 AND ppa.project_id = peia.project_id
                 AND ppa.org_id = hou.org_id
                 AND hou.short_name = nvl(p_ou_name, hou.short_name)
                 and peia.vendor_id = sup.vendor_id(+)
                 and ppa.attribute4 in
                     (Select distinct ppa.segment1
                        from Pa_expenditure_items_all peia,
                             pa_projects_all          ppa,
                             gets_inv_op_unit_info    hou
                       where ppa.project_id = peia.project_id
                         AND ppa.org_id = hou.org_id
                         AND hou.short_name = nvl(p_ou_name, hou.short_name)
                         AND ppa.segment1 =
                             nvl(p_project_number, ppa.segment1)
                         AND ppa.project_type =
                             nvl(p_project_type, ppa.project_type)
                         and trunc(peia.EXPENDITURE_ITEM_DATE) BETWEEN
                             trunc(to_date(p_EXPENDITURE_ITEM_DATE_FROM,
                                           'DD-MON-YY')) AND
                             trunc(to_date(p_EXPENDITURE_ITEM_DATE_TO,
                                           'DD-MON-YY')))
                 and trunc(peia.EXPENDITURE_ITEM_DATE) BETWEEN
                     trunc(to_date(p_EXPENDITURE_ITEM_DATE_FROM, 'DD-MON-YY')) AND
                     trunc(to_date(p_EXPENDITURE_ITEM_DATE_TO, 'DD-MON-YY')));
  
  BEGIN
    dbms_output.put_line('PA Interface Report Procedure started');
    fnd_file.put_line(fnd_file.log,
                      'PA Interface Report Procedure started');
  
    PRINT_XML_TAG(p_xml_tag  => NULL,
                  p_xml_data => '?xml version="1.0" encoding="UTF-8" ?');
    PRINT_XML_TAG(p_xml_tag => NULL, p_xml_data => 'GETS_PA_WARRANTY_RPT');
    FOR rec_data IN c_data LOOP
      PRINT_XML_TAG(p_xml_tag  => NULL,
                    p_xml_data => 'GETS_PA_WARRANTY_RPT_RECORD');
      PRINT_XML_TAG(p_xml_tag  => 'EXPENDITURE_ITEM_ID',
                    p_xml_data => rec_data.EXPENDITURE_ITEM_ID);
      PRINT_XML_TAG(p_xml_tag  => 'PROJECT_NUMBER',
                    p_xml_data => rec_data.PROJECT_NUMBER);
      PRINT_XML_TAG(p_xml_tag  => 'GL_PROJECT_SEGMENT',
                    p_xml_data => rec_data.GL_PROJECT_SEGMENT);
      PRINT_XML_TAG(p_xml_tag  => 'PARENT_PROJECT_NUMBER',
                    p_xml_data => rec_data.PARENT_PROJECT_NUMBER);
      PRINT_XML_TAG(p_xml_tag  => 'PRODUCT_LINE',
                    p_xml_data => rec_data.PRODUCT_LINE);
      PRINT_XML_TAG(p_xml_tag  => 'TASK_NUMBER',
                    p_xml_data => rec_data.TASK_NUMBER);
      PRINT_XML_TAG(p_xml_tag  => 'EXPENDITURE_ITEM_DATE',
                    p_xml_data => to_char(rec_data.EXPENDITURE_ITEM_DATE,
                                          'dd-MON-yyyy'));
      PRINT_XML_TAG(p_xml_tag  => 'EXPENDITURE_TYPE',
                    p_xml_data => rec_data.EXPENDITURE_TYPE);
      PRINT_XML_TAG(p_xml_tag  => 'TRANSACTION_SOURCE',
                    p_xml_data => rec_data.TRANSACTION_SOURCE);
      PRINT_XML_TAG(p_xml_tag  => 'QUANTITY',
                    p_xml_data => rec_data.QUANTITY);
      PRINT_XML_TAG(p_xml_tag  => 'JOB_NAME',
                    p_xml_data => rec_data.JOB_NAME);
      PRINT_XML_TAG(p_xml_tag  => 'RAW_COST_RATE',
                    p_xml_data => rec_data.RAW_COST_RATE);
      PRINT_XML_TAG(p_xml_tag  => 'SUPPLIER_NAME',
                    p_xml_data => rec_data.SUPPLIER_NAME);
      PRINT_XML_TAG(p_xml_tag  => 'MISC_CREDIT_ACCOUNT',
                    p_xml_data => rec_data.MISC_CREDIT_ACCOUNT);
      PRINT_XML_TAG(p_xml_tag  => 'CURRENCY',
                    p_xml_data => rec_data.CURRENCY);
      PRINT_XML_TAG(p_xml_tag  => 'COST_AMOUNT',
                    p_xml_data => rec_data.COST_AMOUNT);
      PRINT_XML_TAG(p_xml_tag  => 'ACCOUNTED_CURRENCY',
                    p_xml_data => rec_data.ACCOUNTED_CURRENCY);
      PRINT_XML_TAG(p_xml_tag  => 'ACCOUNTED_COST',
                    p_xml_data => rec_data.ACCOUNTED_COST);
      PRINT_XML_TAG(p_xml_tag  => 'PO_NUMBER',
                    p_xml_data => rec_data.PO_NUMBER);
      PRINT_XML_TAG(p_xml_tag  => 'PO_LINE_NUMBER',
                    p_xml_data => rec_data.PO_LINE_NUMBER);
      PRINT_XML_TAG(p_xml_tag  => 'WIP_RESOURCE',
                    p_xml_data => rec_data.WIP_RESOURCE);
      PRINT_XML_TAG(p_xml_tag  => 'Item_Number',
                    p_xml_data => rec_data.Item_Number);
      PRINT_XML_TAG(p_xml_tag  => 'COST_DIST_REJECTION_CODE',
                    p_xml_data => rec_data.COST_DIST_REJECTION_CODE);
      PRINT_XML_TAG(p_xml_tag  => 'COST_DISTRIBUTED_FLAG',
                    p_xml_data => rec_data.COST_DISTRIBUTED_FLAG);
      PRINT_XML_TAG(p_xml_tag  => 'GL_PERIOD_NAME',
                    p_xml_data => rec_data.GL_PERIOD_NAME);
      PRINT_XML_TAG(p_xml_tag  => 'DEBIT_CODE_COMBINATION',
                    p_xml_data => rec_data.DEBIT_CODE_COMBINATION);
      PRINT_XML_TAG(p_xml_tag  => 'CREDIT_CODE_COMBINATION',
                    p_xml_data => rec_data.CREDIT_CODE_COMBINATION);
      PRINT_XML_TAG(p_xml_tag  => 'ORIG_TRANSACTION_REFERENCE',
                    p_xml_data => rec_data.ORIG_TRANSACTION_REFERENCE);
      PRINT_XML_TAG(p_xml_tag  => NULL,
                    p_xml_data => '/GETS_PA_WARRANTY_RPT_RECORD');
    END LOOP;
    PRINT_XML_TAG(p_xml_tag => NULL, p_xml_data => '/GETS_PA_WARRANTY_RPT');
  EXCEPTION
    WHEN OTHERS THEN
      errbuf  := substr('Error while calling gets_pa_warranty_rpt_prc' ||
                        SQLERRM,
                        1,
                        250);
      retcode := 2;
      RETURN;
  END gets_pa_warranty_rpt_prc;

END GETS_PA_WARRANTY_PKG;
/