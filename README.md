# c11parser


## Supplementary materials for submission to SAC 2023 

### C11 grammar and automaton
 - production rules : prod_rules.txt
 - automaton (action and goto tables) : action_table.txt, goto_table.txt
 - states : c11.items
 - Reference: [A correct C11 parser written using Menhir and OCaml](https://github.com/jhjourdan/C11parser)

### Syntax completion
 - Learning sets of C11 programs : ansi_c/{cdsa,cJSON,lcc}, gnu_c/{bc-1.07,gzip-1.12,make-4.4,screen-4.9.0,tar-1.34} (https://drive.google.com/file/d/16HRdQUymxbu9W90GyFe7x7lJk_lUybe8/view?usp=sharing)
 - Test sets of C11 programs : ansi_c/kandr (https://drive.google.com/file/d/16HRdQUymxbu9W90GyFe7x7lJk_lUybe8/view?usp=sharing)
 - Candidate database : c11-data-state-collection.txt
 - Analysis result on tutorial programs : c11-analysis-results.txt

### Prototype editor for supporting the syntax completion
 - Note that the editor for MySmallBasic is reused for C11.
   * IMPORTANT: Change Line 53 of src/com/coducation/smallbasic/syncomp/SocketCommunication.java to point to the C11 database as follows:
     * syntaxManager = new SyntaxCompletionDataManager(System.getProperty("user.dir") + "/data/c11/c11-data-collection.txt"); 
   * We are planning to use Emacs as was in the implementation for SCP 2023.
 - https://github.com/kwanghoon/MySmallBasic
 - run java com.coducation.smallbasic.gui.MySmallBasicGUI
 - Enter Ctrl+Space for syntax completion.
 
