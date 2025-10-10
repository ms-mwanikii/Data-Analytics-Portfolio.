SELECT 
       department ,
      avg(salary) as average_salary
FROM employees
GROUP BY department;
