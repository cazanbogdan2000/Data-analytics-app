# Project

The project consists of a data analytics app that will be used to get insights into an (anonymised) dataset containing semester and exam grades from a real lecture. In this project, we will create a Haskell app that will give us a few valuable stats for the course.

The dataset consists of course grades from a real lecture (the names have been changed). The course points were divided into: lecture grades, homework points and exam grades. The dataset has the following problems:

-> lecture grades are mapped against email addresses, whereas homework grades and exam grades are mapped against names.
-> lecture grades also contains entries with no email address. These correspond to students which have not provided an email in a valid form.
-> so, in order to have a unique key for each student, the table email to name student map was created using a form. However, it contains some typos.

### Run checker
make run_test

### Check the output of one task
Load Main.hs into ghci and 
<code>
run_test taskset_number task_number [subtask_number]
</code>
