SELECT 
      count(name) AS no_of_employee_hired_in_2023
FROM employees
WHERE year(hire_date) = 2023
