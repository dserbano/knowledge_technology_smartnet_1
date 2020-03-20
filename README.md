# JOBEX

I implemented an intelligent system that acts as a career advisor. For every user it produces a set of jobs that are best suited for him/her, based on the answers he/she will give to a questionnaire.
Every answer generates 1 or more facts. Every fact has an associated numeric value between -1 and 1 called certainty factor (CF). Through a repeated application of modus ponens the system infers the best set of jobs, also with an associated certainty factor (CF). 

The CLIPS code and documentation is downloadable from http://www.clipsrules.net/. The documentation to consult is mostly:
- User’s Guide: smooth introduction of CLIPS
- Basic Programming Guide: for a reference when detailed information
- https://www.csie.ntu.edu.tw/~sylee/courses/clips/bpg/top.html

To test the program load jobex.clp into CLIPS, reset and run. 

<object data="report.pdf" type="application/pdf" width="700px" height="700px">
    <embed src="report.pdf">
        <p>For more information, read the <a href="report.pdf">Report</a>.</p>
    </embed>
</object>





