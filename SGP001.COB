      ******************************************************************
      * Author: LÁZARO V.A. SANTOS
      * Date: 14-09-2019
      * Purpose: Sistema de pastelaria
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. SGP001.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
        INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO-CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CADCODIGO-CEP
                                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CEPCLIENTE.DAT".
               01 REGCEP.
               03 CODIGO.
                   05 CODIGO-CEP     PIC 9(08).
                03 LOGRADOURO        PIC X(40).
                03 BAIRRO            PIC X(20).


                03 CIDADE            PIC X(20).
                03 UF                PIC X(02).

      *
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELACEP.
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
               USING  CODIGO-CEP
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
        WORKING-STORAGE SECTION.
       01 MASC1        PIC ZZZ.ZZ9,99.
       01 MASC2           PIC ZZZZ.ZZZ.ZZ9,99.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       77 XUF          PIC X(50) VALUE ZEROS.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O PRODUTO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT PRODUTO
                      CLOSE PRODUTO
                      MOVE "*** ARQUIVO PRODUTO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO PRODTUO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CODPRO QUANT PRECMD MASC2 VRESTOQ
                                    ALMOXARIFADO
                MOVE SPACES TO DESCR UNID.
                DISPLAY TELACEP.
       INC-002.
                ACCEPT  TCODPRO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE PRODUTO
                   GO TO ROT-FIM.
                IF CODPRO  = 0
                   MOVE "*** PRODUTO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ PRODUTO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELACEP
                      MOVE "*** PRODUTO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. PRODUTO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 21) DESCR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 21) UNID
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT (07, 21) QUANT
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT (08, 12) PRECMD
                MOVE PRECMD TO MASC1
                DISPLAY (08, 12)MASC1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                COMPUTE VRESTOQ = QUANT * PRECMD
                DISPLAY (09, 21)VRESTOQ.
       INC-008.
                ACCEPT (10, 21) FUNCIONARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-006.
       INC-009.
                ACCEPT (11, 21) ALMOXARIFADO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
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
       INC-WR1.
                WRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** PRODUTO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       LER-XUF.
                MOVE 0 TO W-SEL
                READ XUF
                   IF XUF = "SP"
                      DISPLAY TELACEP
                      MOVE "*** SÃO PAULO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   IF XUF = "AC"
                      DISPLAY TELACEP
                      MOVE "*** ACRE ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   IF XUF = "RJ"
                      DISPLAY TELACEP
                      MOVE "*** RIO DE JANEIRO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   IF XUF = "ES"
                      DISPLAY TELACEP
                      MOVE "*** ESPÍRITO SANTO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ESTADO BRASILEIRO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   NEXT SENTENCE.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
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
       EXC-DL1.
                DELETE PRODUTO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
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
       ALT-RW1.
                REWRITE REGPRO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
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
