create or replace PACKAGE BODY GETS_GL_CCID_STATUS_CHNG_PKG IS
  -- ***********************************************************************************************************************
  --Purpose   :  Custom Web Adi to inactivate CCID.
  --Important :
  --           1. Mandatory parameters          :  CONCATENATED_SEGMENTS,Enabled_Flag
  --           2. Web Adi columns               :  GL_CODE_COMBINATION, ENABLED_FLAG (Y/N)
  --           3. Validation                    :  a) If the combination exists and active, then call gl_code_combinations_pkg.update_row to disable the CCID
  --                                               If the combination doesn’t exist or active, then throw error message “GL combination doesn’t exist/ GL combination is already disabled”.
  --                                               b) If the combination exists and inactive, then call gl_code_combinations_pkg.update_row to enable the CCID
  --                                               If the combination doesn’t exist or inactive, throw error message “GL combination doesn’t exist/ GL combination is already enabled”.

  -- Date            Name            Change Indentifier       Change Description
  -- 26-NOV-2024     Dipika M.        NA                       Base Version
  -- ***********************************************************************************************************************
  PROCEDURE Main(p_gl_code_combination IN VARCHAR2,
                 p_enabled_flag        IN VARCHAR2) IS

    CURSOR c1(x_code_combination_id NUMBER) IS
      SELECT gcc.rowid gcc_rowid, gcc.*
        FROM gl_code_combinations gcc, gl_code_combinations_kfv kfv
       WHERE gcc.code_combination_id = kfv.code_combination_id
         AND gcc.code_combination_id = x_code_combination_id;

    -- Variable Declaration
    v_user_id             NUMBER := g_user_id;
    v_responsibility_id   NUMBER := g_resp_id;
    v_application_id      NUMBER;
    v_upd_count           NUMBER := 0;
    vl_validation_level   NUMBER;
    vl_api_version        NUMBER;
    v_enabled_flag        VARCHAR2(1);
    v_update_flag         VARCHAR2(1);
    v_ccid_exists         NUMBER;
    v_code_combination_id NUMBER;
    v_end_bal             NUMBER;
    --
  BEGIN
    dbms_output.put_line('Before Apps Initialization');
    -- Initialize apps session
    fnd_global.apps_initialize(user_id      => v_user_id,
                               resp_id      => v_responsibility_id,
                               resp_appl_id => g_resp_app_id);

    mo_global.init('SQLAP');
    dbms_output.put_line('After Apps Initialization');
    vl_api_version      := 1.0;
    vl_validation_level := fnd_api.g_valid_level_full;

    -- Check if the code combination exists and get its enabled flag
    BEGIN
      SELECT gcc.enabled_flag, COUNT(1), gcc.code_combination_id
        INTO v_enabled_flag, v_ccid_exists, v_code_combination_id
        FROM gl_code_combinations gcc, gl_code_combinations_kfv kfv
       WHERE gcc.code_combination_id = kfv.code_combination_id
         AND kfv.concatenated_segments = p_gl_code_combination
       GROUP BY gcc.enabled_flag, gcc.code_combination_id;
    EXCEPTION
      WHEN no_data_found THEN
        v_ccid_exists  := 0;
        v_enabled_flag := NULL;
    END;
    --
    v_update_flag := 'Y';
    --
    BEGIN
      SELECT SUM(nvl(gb.begin_balance_dr, 0) - nvl(gb.begin_balance_cr, 0)+
                (nvl(gb.period_net_dr, 0) - nvl(gb.period_net_cr, 0)))
       INTO v_end_bal
      FROM gl_balances gb,gl_code_combinations gcc
       WHERE gb.code_combination_id = gcc.code_combination_id
         AND gb.code_combination_id = v_code_combination_id
         AND gb.period_name IN(
          SELECT a.period_name
           FROM gl_period_statuses a
          WHERE a.ledger_id = gb.ledger_id
            AND a.adjustment_period_flag = 'N'
            AND a.closing_status = 'O'
            AND a.start_date IN(
            SELECT MAX(start_date)
             FROM gl_period_statuses b
            WHERE b.ledger_id = gb.ledger_id
             AND b.adjustment_period_flag = 'N'
             AND closing_status = 'O' ));
     
     EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_update_flag := 'Y';
      WHEN OTHERS THEN
        v_update_flag := 'N';
        RAISE_APPLICATION_ERROR(-20002,
                                  'Exception Occured while Fetching records => ' ||
                                  SQLERRM);
    END;
    -- Checking combination exist or not by passing enabled_flag is 'Y'
    IF p_enabled_flag = 'Y' THEN
      IF v_ccid_exists = 0 THEN
        v_update_flag := 'N';
        DBMS_OUTPUT.PUT_LINE('ERROR : GL combination doesn’t exist.');
        RAISE_APPLICATION_ERROR(-20005, 'GL combination doesn’t exist.');
      END IF;
      --
      IF v_enabled_flag = 'Y' THEN
        v_update_flag := 'N';
        dbms_output.put_line('ERROR : GL combination already enabled.');
        RAISE_APPLICATION_ERROR(-20001, 'GL combination already enabled.');
      END IF;
      -- Checking combination exist or not by passing enabled_flag is 'N'
    ELSIF p_enabled_flag = 'N' THEN
      IF v_ccid_exists = 0 THEN
        v_update_flag := 'N';
        DBMS_OUTPUT.PUT_LINE('ERROR : GL combination doesn’t exist.');
        RAISE_APPLICATION_ERROR(-20006, 'GL combination doesn’t exist.');
      END IF;
      --
      IF v_enabled_flag = 'N' THEN
        v_update_flag := 'N';
        dbms_output.put_line('ERROR : GL combination already disabled.');
        RAISE_APPLICATION_ERROR(-20002, 'GL combination already disabled.');
      END IF;
      -- Checking balance exist or not for Combination
      IF v_end_bal > 0 OR v_end_bal < 0 THEN
         v_update_flag := 'N';
         RAISE_APPLICATION_ERROR(-20001, 'Balance exist for the specified GL Code Combination.');
      END IF;
      --
    END IF;
    --
    IF v_update_flag = 'Y' THEN
      FOR r1 IN c1(v_code_combination_id) LOOP
        BEGIN
          DBMS_OUTPUT.PUT_LINE('call gl_code_combinations_pkg.update_row to enabled/disable the CCID');
          gl_code_combinations_pkg.update_row(x_rowid                   => r1.gcc_rowid,
                                              x_code_combination_id     => r1.code_combination_id,
                                              x_alt_code_combination_id => r1.alternate_code_combination_id,
                                              x_last_update_date        => sysdate,
                                              x_last_updated_by         => v_user_id,
                                              x_chart_of_accounts_id    => r1.chart_of_accounts_id,
                                              x_detail_posting_f        => r1.detail_posting_allowed_flag,
                                              x_detail_budgeting_f      => r1.detail_budgeting_allowed_flag,
                                              x_balanced_budgetf        => r1.igi_balanced_budget_flag,
                                              x_account_type            => r1.account_type,
                                              x_enabled_flag            => p_enabled_flag,
                                              x_summary_flag            => r1.summary_flag,
                                              x_segment1                => r1.segment1,
                                              x_segment2                => r1.segment2,
                                              x_segment3                => r1.segment3,
                                              x_segment4                => r1.segment4,
                                              x_segment5                => r1.segment5,
                                              x_segment6                => r1.segment6,
                                              x_segment7                => r1.segment7,
                                              x_segment8                => r1.segment8,
                                              x_segment9                => r1.segment9,
                                              x_segment10               => r1.segment10,
                                              x_segment11               => r1.segment11,
                                              x_segment12               => r1.segment12,
                                              x_segment13               => r1.segment13,
                                              x_segment14               => r1.segment14,
                                              x_segment15               => r1.segment15,
                                              x_segment16               => r1.segment16,
                                              x_segment17               => r1.segment17,
                                              x_segment18               => r1.segment18,
                                              x_segment19               => r1.segment19,
                                              x_segment20               => r1.segment20,
                                              x_segment21               => r1.segment21,
                                              x_segment22               => r1.segment22,
                                              x_segment23               => r1.segment23,
                                              x_segment24               => r1.segment24,
                                              x_segment25               => r1.segment25,
                                              x_segment26               => r1.segment26,
                                              x_segment27               => r1.segment27,
                                              x_segment28               => r1.segment28,
                                              x_segment29               => r1.segment29,
                                              x_segment30               => r1.segment30,
                                              x_description             => r1.description,
                                              x_template_id             => r1.template_id,
                                              x_start_date_active       => r1.start_date_active,
                                              x_end_date_active         => r1.end_date_active,
                                              x_attribute1              => r1.attribute1,
                                              x_attribute2              => r1.attribute2,
                                              x_attribute3              => r1.attribute3,
                                              x_attribute4              => r1.attribute4,
                                              x_attribute5              => r1.attribute5,
                                              x_attribute6              => r1.attribute6,
                                              x_attribute7              => r1.attribute7,
                                              x_attribute8              => r1.attribute8,
                                              x_attribute9              => r1.attribute9,
                                              x_attribute10             => r1.attribute10,
                                              x_context                 => r1.context,
                                              x_segment_attribute1      => r1.segment_attribute1,
                                              x_segment_attribute2      => r1.segment_attribute2,
                                              x_segment_attribute3      => r1.segment_attribute3,
                                              x_segment_attribute4      => r1.segment_attribute4,
                                              x_segment_attribute5      => r1.segment_attribute5,
                                              x_segment_attribute6      => r1.segment_attribute6,
                                              x_segment_attribute7      => r1.segment_attribute7,
                                              x_segment_attribute8      => r1.segment_attribute8,
                                              x_segment_attribute9      => r1.segment_attribute9,
                                              x_segment_attribute10     => r1.segment_attribute10,
                                              x_segment_attribute11     => r1.segment_attribute11,
                                              x_segment_attribute12     => r1.segment_attribute12,
                                              x_segment_attribute13     => r1.segment_attribute13,
                                              x_segment_attribute14     => r1.segment_attribute14,
                                              x_segment_attribute15     => r1.segment_attribute15,
                                              x_segment_attribute16     => r1.segment_attribute16,
                                              x_segment_attribute17     => r1.segment_attribute17,
                                              x_segment_attribute18     => r1.segment_attribute18,
                                              x_segment_attribute19     => r1.segment_attribute19,
                                              x_segment_attribute20     => r1.segment_attribute20,
                                              x_segment_attribute21     => r1.segment_attribute21,
                                              x_segment_attribute22     => r1.segment_attribute22,
                                              x_segment_attribute23     => r1.segment_attribute23,
                                              x_segment_attribute24     => r1.segment_attribute24,
                                              x_segment_attribute25     => r1.segment_attribute25,
                                              x_segment_attribute26     => r1.segment_attribute26,
                                              x_segment_attribute27     => r1.segment_attribute27,
                                              x_segment_attribute28     => r1.segment_attribute28,
                                              x_segment_attribute29     => r1.segment_attribute29,
                                              x_segment_attribute30     => r1.segment_attribute30,
                                              x_segment_attribute31     => r1.segment_attribute31,
                                              x_segment_attribute32     => r1.segment_attribute32,
                                              x_segment_attribute33     => r1.segment_attribute33,
                                              x_segment_attribute34     => r1.segment_attribute34,
                                              x_segment_attribute35     => r1.segment_attribute35,
                                              x_segment_attribute36     => r1.segment_attribute36,
                                              x_segment_attribute37     => r1.segment_attribute37,
                                              x_segment_attribute38     => r1.segment_attribute38,
                                              x_segment_attribute39     => r1.segment_attribute39,
                                              x_segment_attribute40     => r1.segment_attribute40,
                                              x_segment_attribute41     => r1.segment_attribute41,
                                              x_segment_attribute42     => r1.segment_attribute42,
                                              x_jgzz_recon_context      => r1.jgzz_recon_context,
                                              x_jgzz_recon_flag         => r1.jgzz_recon_flag,
                                              x_reference1              => r1.reference1,
                                              x_reference2              => r1.reference2,
                                              x_reference3              => r1.reference3,
                                              x_reference4              => r1.reference4,
                                              x_reference5              => r1.reference5,
                                              x_preserve_flag           => r1.preserve_flag,
                                              x_refresh_flag            => r1.refresh_flag);

          v_upd_count := v_upd_count + 1;

        END;
      END LOOP;
      dbms_output.put_line('v_upd_count:' || v_upd_count);
    END IF;
    COMMIT;
    --
  EXCEPTION
    WHEN OTHERS THEN
      raise_application_error(-20001,
                              'Error in procedure Main => : ' ||
                              ' ERROR=> ' || SQLERRM);
  END MAIN;
END GETS_GL_CCID_STATUS_CHNG_PKG;
/