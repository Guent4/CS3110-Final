WELCOME TO OASys

FEATURE DESCRIPTIONS
The OASys specification is similar to that of Git. OASys is composed of a frontend and backend system. Similar to Git, users can type “oasys add .” (after installing oasys of course), in order to add files to the staging area. Commands range from rudimentary ones such as “init”, “add”, and “commit” to more advanced ones like “pull,” “push,” and “merge”.  Branching will be supported because it is a vital part of version control when working together with a team.
A REPL was not implemented because it makes interacting with OASys more tedious. Every time the user wants to use OASys, they need to start it up and input commands to the REPL; then when they are done with OASys, they need to quit before using the normal Terminal commands like “ls.”  The front-end, nicknamed Camel, takes in the user input and parses it, provide searching capabilities, and provide suggestions for mistyped words.  After execution of the user’s desired action is complete, Camel prints out the feedback from the execution.
An additional feature of OASys allows for the communication between local and remote repositories. Remote repositories can be hosted on Cornell Servers. This could offer benefits to both the student and course staff.  The students would get to use a customized and wrapped version control.  The course staff would be able to easily monitor the progress and source control etiquette of students for any given assignment. Additionally, ensuring that students use private Git repositories will no longer be a concern for the course staff since the repositories containing the students’ code will be hosted privately on the Cornell servers.


STANDARD OPERATING PROCEDURES
Installing
- “cd” to the home location of OASys and then run bin/install; when done, restart Terminal
- Now you should be able to run “oasys ____” anywhere because running oasys got added to the .bashrc file
Using Oasys:
- A server must be running for some of the functionality to work. In a terminal window, enter “bin/run_server” and leave the window open.
- Open a new terminal window for further operations.
- You can run commands by typing “oasys ____”
- For help regarding certain commands, type “oasys help --cmd insert_command_name”
- For general searching of some popular questions/topics about VC type “oasys help search words” where “search words” are words to be searched for
Running our tests:
- Open new window and call “bin/run_server”
- Enter “bin/run_tests” (NOTE: FAILURE ___ statements will be printed; these are normal and are part of the testing process)
Uninstalling
- Run “oasys_uninstall” anywhere (does not depend on file directory); restart Terminal for changes to be applied
