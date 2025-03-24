create or replace package body gets_emp_salary_pkg IS
-- ***********************************************************************************************************************
-- TO Calculate total salary of employee department wise
  -- Date            Name            Change Indentifier       Change Description
  -- 24-MAR-2025    Dipika M.        NA                       Base Version
  -- LINE 36
  -- TIME 1 HOURS
-- ***********************************************************************************************************************
 FUNCTION gets_total_sal(p_department_id NUMBER)
   RETURN NUMBER 
   IS
   v_total_sal NUMBER := 0;
 BEGIN
   SELECT SUM(salary)
      into v_total_sal
    from Employee_DM
  where DEPARTMENT_ID = p_department_id;

    RETURN v_total_sal;
  EXCEPTION
   WHEN NO_DATA_FOUND THEN
    RETURN 0;
   WHEN OTHERS THEN
    RETURN -1;
 END gets_total_sal;
   PROCEDURE gets_display_sal_ IS
    dept_rec Employee_DM.DEPARTMENT_ID%type;
      v_total NUMBER;
  BEGIN
    FOR dept_rec IN (select distinct DEPARTMENT_ID from Employee_DM) LOOP
     v_total := gets_total_sal(dept_rec.DEPARTMENT_ID);

     DBMS_OUTPUT.PUT_LINE('Department ID: '  || dept_rec.DEPARTMENT_ID || '- Total Salary: ' || v_total);
  END LOOP;
   END gets_display_sal_;
 END gets_emp_salary_pkg;