# ASSIGNMENT I 

Using CLIPS 6.3, create a knowledge-base system with support to reasoning under uncertainty. After choosing a suitable problem, decide how to implement the uncertainty methodology into CLIPS and create a CLIPS rule base to represent the problem's knowledge base and experiment by consult it to ensure that it properly functions.

To be included:
- Description of the implementation of the uncertainty manipulation in your system
- Problem's description and some example runs
- CLIPS code with comments

The CLIPS code and documentation is downloadable from http://www.clipsrules.net/. The documentation to consult is mostly:
- User’s Guide: smooth introduction of CLIPS
- Basic Programming Guide: for a reference when detailed information
- https://www.csie.ntu.edu.tw/~sylee/courses/clips/bpg/top.html

# SOLUTION: JOBEX

I implemented an intelligent system that acts as a career advisor. For every user it produces a set of jobs that are best suited for him/her, based on the answers he/she will give to a questionnaire.
Every answer generates 1 or more facts. Every fact has an associated numeric value between -1 and 1 called certainty factor (CF). Through a repeated application of modus ponens the system infers the best set of jobs, also with an associated certainty factor (CF). 

To test the program load jobex.clp into CLIPS, reset and run. 

<object data="report.pdf" type="application/pdf" width="700px" height="700px">
    <embed src="report.pdf">
        <p>For more information, read the <a href="report.pdf">Report</a>.</p>
    </embed>
</object>





