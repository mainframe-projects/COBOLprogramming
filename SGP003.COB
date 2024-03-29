      ******************************************************************
      * Author: LÁZARO V.A. SANTOS
      * Date: 28-09-2019
      * Purpose: Cadastro de Cliente
      * Tectonics: COB
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGP003.
      **************************************
      * MANUTENCAO DO CADASTRO DE CLIENTE  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCLIENTE ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS REGCLI
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCLIENTE
      *         LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCLIENTE.DAT".
       01 REGCLI.
                03 CPF               PIC X(12).
                03 NOME              PIC X(40).
                03 RG                PIC X(15).
                03 ESTADOCIVIL       PIC X(12).
                03 CEP               PIC 9(1).
                03 LOGRADOURO        PIC X(40).
                03 COMPLE            PIC X(10).
                03 TELEFONE          PIC 9(9).
                03 NUM               PIC X(13).
                03 BAIRRO            PIC X(25).
                03 CIDADE            PIC X(25).
                03 UF                PIC X(2).
                03 EMAIL             PIC X(40).
                03 SITUACAO          PIC X(20).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.

       01 TABAMIGO.
           03 T4 PIC X(55) VALUE
           "FAXULDADE  IGREJA     BAIRRO     BAR        ACADEMIA   ".
           03 T5 PIC X(56) VALUE
           "COLEGIO    SHOW       FAMILIA    NAMORADA(O)".
       01 TABAUX REDEFINES TABAMIGO.
           03 TAMIGO        PIC X(11) OCCURS 09 TIMES.
       01 ALFAAMIGO         PIC X(11).

           01 TABGENERO.
           03 T1 PIC X(36) VALUE
           "MMAS  FFEM TTRANS   SSEMGEN".
           03 T2 PIC X(36) VALUE
           "AWWW".
       01 TABGEN REDEFINES TABGENERO.
           03 TGEN        PIC X(11) OCCURS 10 TIMES.
       01 ALFAGN.
             03 ALFAGN1 PIC X(01).
             03 ALFAGN2 PIC X(11).
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-000.
           OPEN I-O CADCLIENTE
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCLIENTE
                      CLOSE CADCLIENTE
                      MOVE "*** ARQUIVO CADAMIGO FOI CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-000
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQ CADAMIGO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      GO TO INC-001.
      *               NEXT SENTENCE.
       INC-001.
      *          DISPLAY (01, 01) ERASE.
                DISPLAY "CADASTRO DE AMIGOS" AT 0120
                DISPLAY "APELIDO            : " AT 0401
                DISPLAY "NOME               : " AT 0501
                DISPLAY "EMAIL              : " AT 0601
                DISPLAY "DATA DE NASCIMENTO : " AT 0701
                DISPLAY "TIPO DE AMIGO      : " AT 0801
                DISPLAY "SEXO               : " AT 0901
                DISPLAY "GENERO AMIGO       : " AT 1001
                DISPLAY "TELEFONE           : " AT 1101.


       INC-002.
                ACCEPT CADCLIENTE AT 0421.
      *          ACCEPT W-ACT FROM ESCAPE KEY.
                 IF W-ACT = 02
                   CLOSE CADCLIENTE
                   GO TO ROT-FIM.
                IF CADCLIENTE = " "
                   MOVE "*** NOME INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ CADAMIGO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (05, 21) NOME
                      DISPLAY (06, 21) EMAIL
                      DISPLAY (07, 21) DATA-NASC
                      DISPLAY (08, 21) TIPO-AMIGO
                      DISPLAY (09, 21) SEXO
                      DISPLAY (10, 21) GENERO
                      DISPLAY (11, 21) TELEFONE
                      MOVE "*** NOME JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADAMIGO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 21) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 21) EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.

       INC-005.
                ACCEPT (06, 21) DATA-NASC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT (06, 21) TIPO-AMIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF TIPO-AMIGO = 0 OU TIPO-AMIGO > 9
                 MOVE "TIPO DE AMIGO INVALIDO" TO MENS
                 PEFORM ROT-MENS TO ROT-MENS-FIM
                 GO TO INC-006.
                 MOVE TAMIGO(TIPO-AMIGO) TO ALFAAMIGO
                 DISPLAY (LL CC) ALFAAMIGO.
       INC-007.
                ACCEPT (06, 21) SEXO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT (06, 21) GENERO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                MOVE 1 TO IND.
       INC-008A.
                MOVE TABGEN(IND) TO ALFAGN
                IF ALFAGN1 NOT = GENERO
                   ADD 1 TO IND
                     IF IND > 10
                       MOVE "*** GENERO NAO ENCONTRADO ***" TO MENS
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO INC-008
                     ELSE
                       GO TO INC=008A
                ELSE
                     DISPLAY (LL CC) ALFAGN2
       INC-009.
                ACCEPT (06, 21) TELEFONE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-001.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** APELIDO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADAMIGO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
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
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADAMIGO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
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
                IF W-ACT = 02 GO TO INC-003.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
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
