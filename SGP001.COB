      ******************************************************************
      * Author: LÁZARO V.A. SANTOS
      * Date: 14-09-2019
      * Purpose: Cadastro de CEP
      * Tectonics: COB
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. SGP001.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO "CADCEP.DAT"
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS LOGRADOURO
                                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.

       FD CADCEP.

       01 REGCEP.
               03 CODCEP        PIC 9(08).
               03 LOGRADOURO        PIC X(40).
               03 BAIRRO            PIC X(20).
               03 CIDADE            PIC X(20).
               03 UF                PIC X(02).

      *
       WORKING-STORAGE SECTION.

       01 MASC1           PIC ZZZ.ZZ9,99.
      * 01 MASC2           PIC ZZZZ.ZZZ.ZZ9,99.

       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.

       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 XUF          PIC X(20) VALUE ZEROS.
      *----------------------------------------------------------------*
       01 IND PIC 9(02) VALUE ZEROS.

       01 TB.
       03 TB1 PIC X(54) VALUE "ACAMROSPRJBASCMTMGRSESCEAL".

       01 TABUF REDEFINES TB.
       03 TABUFX PIC X(02) OCCURS 27 TIMES.
       01 TBE.
      *                         1234567890123456789012345678901234567890
       03 TBE1 PIC X(40) VALUE "SÃO PAULO           ACRE".
       03 TBE2 PIC X(40) VALUE "RIO DE JANEIRO      CEARA".
       03 TBE3 PIC X(40) VALUE "ESPIRITO SANTO      BAHIA".
       03 TBE4 PIC X(40) VALUE "AMAZONAS".
       03 TBE5 PIC X(40) VALUE "MINAS GERAIS".
       03 TBE6 PIC X(40) VALUE "BAHIA".
       03 TBE7 PIC X(40) VALUE "CEARÁ".
       03 TBE8 PIC X(40) VALUE "ALAGOAS".
       03 TBE9 PIC X(40) VALUE "SANTA CATARINA".
       03 TBE10 PIC X(20) VALUE "BRASÍLIA".
       03 TBE11 PIC X(20) VALUE "DISTRITO FEDERAL".
       03 TBE12 PIC X(20) VALUE "MARANHÃO".
       03 TBE13 PIC X(20) VALUE "PIAUÍ".
       03 TBE14 PIC X(20) VALUE "AMAPÁ".
       03 TBE15 PIC X(20) VALUE "RONDÔNIA".
       03 TBE16 PIC X(20) VALUE "RORAIMA".

       01 TABUFE REDEFINES TBE.

       03 TABUFE1 PIC X(20) OCCURS 27 TIMES.

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA.
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
               USING  CODCEP
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 07  COLUMN 25  PIC X(40)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 09  COLUMN 25  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.

           05  TCIDADE
               LINE 11  COLUMN 30  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 13  COLUMN 26  PIC X(02)
               USING  UF
               HIGHLIGHT.
            05 TUFX
               LINE 13  COLUMN 29  PIC X(20)
               USING  XUF
               HIGHLIGHT.
      *-----------------------------------------------------------------

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

       INICIO.
       INC-001.
               DISPLAY TELA.

       INC-002.
                ACCEPT CODCEP AT 0421
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADCEP
                   GO TO ROT-FIM.
                IF CODCEP  = 0
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-CEP01.
                MOVE 0 TO W-SEL
                READ CADCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY CODCEP AT 0512
                      DISPLAY LOGRADOURO AT 0612
                      DISPLAY BAIRRO AT 0712
                      DISPLAY CIDADE AT 0812
                      DISPLAY UF AT 0912
                      MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. PRODUTO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   GO TO INC-003.
       INC-003.
                ACCEPT CODCEP AT 0521
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT LOGRADOURO AT 0621
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT BAIRRO AT 0721
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT UF
                MOVE UF TO MASC1
                DISPLAY MASC1 AT 0812.
      *          ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-006A.
               MOVE 1 TO IND
               GO TO INC-007.

       INC-007.
               IF UF NOT = TABUFX (IND)
                   IF IND < 27
                       ADD 1 TO IND
                       GO TO INC-006
                       ELSE
                           DISPLAY "UF INCORRETA" AT 2340
                           GO TO INC-006
                           ELSE
                           DISPLAY TELA.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY "DADOS OK (S/N) : " AT 2340
                ACCEPT W-OPCAO AT 2357.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.

       ACE-001.
                DISPLAY
             "F1=NOVO REGISTRO DE CEP   F2=ALTERAR CEP  F3=EXCLUIR CEP"
              AT 2312.
                ACCEPT W-OPCAO AT 2355.
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY MENS AT 2312.
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY "EXCLUIR   (S/N) : " AT 2340.
                ACCEPT W-OPCAO AT 2357.
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
                DISPLAY "ALTERAR  (S/N) : " AT 2340.
                ACCEPT W-OPCAO AT 2357.
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

                DISPLAY "..." AT 2340.
                ACCEPT W-OPCAO AT 2357.
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
      *          DISPLAY ERASE AT 0101.
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
               DISPLAY MENS at 2312.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 30000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY LIMPA AT 2312.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

       MAIN-PROCEDURE.
            DISPLAY "BEM-VINDO AO SGP!"
            STOP RUN.
       END PROGRAM SGP001.
