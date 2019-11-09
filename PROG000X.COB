
      *----------------------------------------------------------------*
       IDENTIFICATION                          DIVISION.
      *================================================================*

       PROGRAM-ID. PROG0001.

      *************DOCUMENTACAO*****************************************
      ******************************************************************
      * Author: LÁZARO V.A. SANTOS
      * Date:09-14-2019
      * Purpose: Treinanmento de COBOL
      * Tectonics: cobc
      ******************************************************************
      *********************************FIM***DOCUMENTACAO***************
      *================================================================*
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  NUMEROX                     PIC 9(18).
       77  NUMEROX-SINAL               PIC S9(18).
       77  NUMEROX-DECIMAL             PIC 9(07)V99.
       77  NOMEX                       PIC A(20).
       77  NOMENUMERO                  PIC X(30).

       01  CLIENTE.
               02 NOME         PIC X(50) VALUE "MÔNICA DE SOUZA".
               02 RG           PIC X(11) VALUE "11122233344".
               02 CPF          PIC X(11) VALUE "11122233344".
               02 ENDERECO     PIC X(20) VALUE "RUA DO LIMOEIRO".
               02 DATANASC.
                   03 DIA      PIC XX.
                   03 MES      PIC XX.
                   03 ANO      PIC X(04).

      *----------------------------------------------------------------*


       PROCEDURE DIVISION.
      *================================================================*

                DISPLAY CLIENTE AT 0101
                DISPLAY DIA AT 0102
                DISPLAY MES AT 0103
                DISPLAY ANO AT 0104.
                STOP RUN.
