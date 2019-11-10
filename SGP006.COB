      ******************************************************************
      * Author: LÁZARO VINICIUS A. SANTOS
      * Date: 09/11/2019
      * Purpose: SISTEMA CADASTRO DE PEDIDO
      * Tectonics: cobc
      ******************************************************************
              IDENTIFICATION DIVISION.
       PROGRAM-ID. SGP006.

      **************************************
      * MANUTENCAO DO CADASTRO DE PRODUTO  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADPED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS NOMECLIENTE
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS PRODUTO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPED.DAT".
       01 ARQPED.
                03 PEDIDO             PIC 9(05).
                03 CLIENTE.
                       04 NOMECLIENTE PIC X(05).
                       04 CPF         PIC X(11).
                03 PRODUTO              PIC X(30).
                03 UNIDADE              PIC X(10).
                03 PRECOVENDA           PIC X(10).
                03 QUANTIDADE           PIC X(05).
                03 VALORPEDIDO          PIC X(10).

                03 CALCPRECO             PIC X(10) VALUE ZEROS.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.


       SCREEN SECTION.

       01 TELA.
           03 BLANK SCREEN.

           03 LINE 01 COLUMN 20 "CADASTRO DE PEDIDOS"
           03 LINE 02 COLUMN 20 "PEDIDO             "
           03 LINE 03 COLUMN 20 "CLIENTE NOME       "
           03 LINE 04 COLUMN 20 "CPF                "
           03 LINE 05 COLUMN 20 "PRODUTO            "
           03 LINE 06 COLUMN 20 "UNIDADE            "
           03 LINE 07 COLUMN 20 "PRECOVENDA         "
           03 LINE 08 COLUMN 20 "QUANTIDADE         "
           03 LINE 09 COLUMN 20 "VALOR DO PEDIDO    "



      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-000.
           OPEN I-O CADPED
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADPED
                      CLOSE CADPED
                      MOVE "*** ARQUIVO CADPED FOI CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-000
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQ CADPED" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    GO TO INC-001.
      *                 NEXT SENTENCE.
       INC-001.
                DISPLAY TELA.



       INC-002.
                ACCEPT  (04, 21) NOMECLIENTE
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADPED
                   GO TO ROT-FIM.
                IF NOMECLIENTE  = " "
                   MOVE "*** NOME INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ CADPED
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (05, 21) PEDIDO
                      DISPLAY (06, 21) NOMECLIENTE
					               DISPLAY (07, 21) CPF
				                 DISPLAY (08, 21) PRODUTO
				                 DISPLAY (09, 21) UNIDADE
				                 DISPLAY (10, 21) PRECOVENDA
				                 DISPLAY (11, 21) QUANTIDADE
                      DISPLAY (12, 21) VALORPEDIDO
                      MOVE "*** PEDIDO JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CADPED"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 21) NOMECLIENTE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 21) PEDIDO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.

	      INC-005.
                ACCEPT (06, 21) CPF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
	      INC-006.
                ACCEPT (06, 21) PRODUTO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
	      INC-007.
                ACCEPT (06, 21) UNIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
	      INC-008.
                ACCEPT (06, 21) PRECOVENDA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
			         MOVE 1 TO IND.
       INC-008A.
                MOVE TABGEN(IND) TO QUANTIDADE
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
       INC-010.
               ACCEPT (06,21) QUANTIDADE
               ACCEPT W-ACT FROM ESCAPE KEY
               ADD PRECOVENDA TO QUANTIDADE
               GO TO INC-001.

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
                WRITE REGPED
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** APELIDO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CADPED"
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
