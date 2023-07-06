# c11parser


## For supplementary materials for submission to SLE 2023 

### C11 grammar and automaton
 - production rules : prod_rules.txt
 - automaton (action and goto tables) : action_table.txt, goto_table.txt
 - states : c11.items
 - Reference: [A correct C11 parser written using Menhir and OCaml](https://github.com/jhjourdan/C11parser)

### Syntax completion
 - Learning sets of C11 programs : ansi_c/{cdsa,cJSON,lcc}, gnu_c/{bc-1.07,gzip-1.12,make-4.4,screen-4.9.0,tar-1.34} (https://drive.google.com/file/d/17NPDF47R5truECDfSuBLnNl2jdVon3r1/view?usp=sharing)
 - Test sets of C11 programs : ansi_c/kandr (https://drive.google.com/file/d/17NPDF47R5truECDfSuBLnNl2jdVon3r1/view?usp=sharing)
 - Candidate database : c11-data-state-collection.txt
 - Analysis result on tutorial programs : C_anal.txt

### Prototype editor for supporting the syntax completion
 - Note that the editor for MySmallBasic is reused for C11.
 - https://github.com/kwanghoon/MySmallBasic
 - run java com.coducation.smallbasic.gui.MySmallBasicGUI
 - Enter Ctrl+Space for syntax completion.
 
