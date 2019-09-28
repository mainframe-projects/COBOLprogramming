      ******************************************************************
      * Author: LÁZARO V.A. SANTOS
      * Date: 21-09-2019
      * Purpose: Sistema de pastelaria
      * Tectonics: COB
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. SGP002.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADPRO ASSIGN TO "CADPRO.DAT"
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CODIGO
                                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

       FD CADPRO.

       01 REGPRO.
               03 CODIGO       PIC 9(08).
               03 DENOMINACAO        PIC X(30).
               03 UNIDADE            PIC X(12).
               03 PRECOCUSTO            PIC 9(06).
               03 PRECOVENDA            PIC 9(06).
               03 TIPOPRODUTO           PIC 9(15).
               03 SITUACAO               PIC X(12).

      *        TODOS OS CAMPOS CONSISTIDOS (NÃO PODE ZEROS OU BRANCOS)
       WORKING-STORAGE SECTION.

       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.

       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 XUF          PIC X(20) VALUE ZEROS.
      *----------------------------------------------------------------*

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPRO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "|---------------------------------------".
           05  LINE 01  COLUMN 41
               VALUE  "---------------------------------------|".
           05  LINE 02  COLUMN 01
               VALUE  "|                            CADASTRO DE".
           05  LINE 02  COLUMN 41
               VALUE  " CEP                                   |".
           05  LINE 03  COLUMN 01
               VALUE  "|".
           05  LINE 03  COLUMN 41
               VALUE  "                                       |".
           05  LINE 04  COLUMN 01
               VALUE  "|".
           05  LINE 04  COLUMN 41
               VALUE  "                                       |".
           05  LINE 05  COLUMN 01
               VALUE  "|   CÓDIGO".
           05  LINE 05  COLUMN 41
               VALUE  "                                       |".
           05  LINE 06  COLUMN 01
               VALUE  "|".
           05  LINE 06  COLUMN 41
               VALUE  "                                       |".
           05  LINE 07  COLUMN 01
               VALUE  "|   LOGRADOURO".
           05  LINE 08  COLUMN 01
               VALUE  "|".
           05  LINE 09  COLUMN 01
               VALUE  "|   BAIRRO".
           05  LINE 10  COLUMN 01
               VALUE  "|".
           05  LINE 11  COLUMN 01
               VALUE  "|   CIDADE".
           05  LINE 12  COLUMN 01
               VALUE  "|".
           05  LINE 13  COLUMN 01
               VALUE  "|   -".
           05  LINE 14  COLUMN 01
               VALUE  "|".
           05  LINE 15  COLUMN 01
               VALUE  "|   -".
           05  LINE 16  COLUMN 01
               VALUE  "|".
           05  LINE 17  COLUMN 01
               VALUE  "|   UF".
           05  LINE 17  COLUMN 41
               VALUE  "                                       |".
           05  LINE 18  COLUMN 01
               VALUE  "|".
           05  LINE 18  COLUMN 41
               VALUE  "                                       |".
           05  LINE 19  COLUMN 01
               VALUE  "|".
           05  LINE 19  COLUMN 41
               VALUE  "                                       |".
           05  LINE 20  COLUMN 01
               VALUE  "|".
           05  LINE 20  COLUMN 41
               VALUE  "                                       |".
           05  LINE 21  COLUMN 01
               VALUE  "|".
           05  LINE 21  COLUMN 41
               VALUE  "                                       |".
           05  LINE 22  COLUMN 01
               VALUE  "|".
           05  LINE 22  COLUMN 41
               VALUE  "                                       |".
           05  LINE 23  COLUMN 01
               VALUE  "|   MENSAGEM".
           05  LINE 23  COLUMN 41
               VALUE  "                                       |".
           05  LINE 24  COLUMN 01
               VALUE  "|_______________________________________".
           05  LINE 24  COLUMN 41
               VALUE  "_______________________________________|".
           05  TCODIGO
               LINE 05  COLUMN 25  PIC 99999.999
               USING  CODIGO
               HIGHLIGHT.
           05  TCPRECOCUSTO
               LINE 05  COLUMN 25  PIC 99999.999
               USING  PRECOCUSTO
               HIGHLIGHT.
           05  TPRECOVENDA
               LINE 05  COLUMN 25  PIC 99999.999
               USING  PRECOVENDA
               HIGHLIGHT.
           05  TDENOMINACAO
               LINE 07  COLUMN 25  PIC X(30)
               USING  DENOMINACAO
               HIGHLIGHT.
           05  TUNIDADE
               LINE 09  COLUMN 25  PIC X(12)
               USING  UNIDADE
               HIGHLIGHT.
           05  TTIPOPRODUTO
               LINE 11  COLUMN 30  PIC X(20)
               USING  TIPOPRODUTO
               HIGHLIGHT.
           05  TSITUACAO
               LINE 13  COLUMN 26  PIC X(02)
               USING  SITUACAO
               HIGHLIGHT.
            05 TUFX
               LINE 13  COLUMN 29  PIC X(20)
               USING  XUF
               HIGHLIGHT.
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       INICIO.
      *



       INC-006.

       INC-007.

       INC-008.

       INC-009.


       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY "DADOS OK (S/N) : " AT 2340.
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.

       ACE-001.
                DISPLAY (23, 12)
            "F1=NOVO REGISTRO DE CEP   F2=ALTERAR CEP  F3=EXCLUIR CEP"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.

      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.

        DISPLAY (23, 40) "...".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "0" OR "Þ"
                   MOVE "*** CARACTERES NAO ACEITOS ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 30000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

       MAIN-PROCEDURE.
            DISPLAY "BEM-VINDO AO SGP!"
            STOP RUN.
       END PROGRAM SGP001.
