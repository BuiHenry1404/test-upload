000010*     SMPM002           TEMPORARY LEVEL=1    DATE=26.03.18
000020*****************************************************************
000030*                                                               *
000040*    CHUC NANG  : XU LY GIAO DICH CHUYEN KHOAN                 *
000050*                 Fund Transfer Transaction Processing           *
000060*                                                               *
000070*    PROGAM-ID :  SMPM002                                       *
000080*                                                               *
000090*    UPDATE HISTORY                                             *
000100*       CREATE  26.03.18                                        *
000110*       UPDATE                                                  *
000120*                                                               *
000130*****************************************************************
000140 IDENTIFICATION          DIVISION.
000150 PROGRAM-ID.             SMPM002.
000160*
000170 ENVIRONMENT             DIVISION.
000180 CONFIGURATION           SECTION.
000190 SOURCE-COMPUTER.        FACOM.
000200 OBJECT-COMPUTER.        FACOM.
000210 INPUT-OUTPUT            SECTION.
000220 FILE-CONTROL.
000230     SELECT        IN01-FILE      ASSIGN  TO  IN01.
000240     SELECT        OT01-FILE      ASSIGN  TO  OT01.
000250     SELECT        OT02-FILE      ASSIGN  TO  OT02.
000260*
000270*****************************************************************
000280 DATA                    DIVISION.
000290*****************************************************************
000300 FILE                    SECTION.
000310*
000320 FD  IN01-FILE.
000330 01  IN01-REC.
000340     COPY  SMPC002    REPLACING  ==(*)==  BY  ==IN01==.
000350*
000360 FD  OT01-FILE
000370              BLOCK       CONTAINS    0       RECORDS.
000380 01  OT01-REC.
000390     COPY  SMPC002    REPLACING  ==(*)==  BY  ==OT01==.
000400*
000410 FD  OT02-FILE
000420              BLOCK       CONTAINS    0       RECORDS.
000430 01  OT02-REC.
000440     03  OT02-TRAN-NO               PIC  X(015).
000450     03  OT02-ERROR-MSG             PIC  X(200).
000460*
000470*****************************************************************
000480 WORKING-STORAGE         SECTION.
000490*****************************************************************
000500*
000510 01  FLAG-AREA.
000520     03  END-FLAG                   PIC X(003) VALUE SPACE.
000530         88  WK-END                 VALUE "END".
000540     03  ERR-FLAG                   PIC X(001) VALUE SPACE.
000550         88  ERR-YES                VALUE "Y".
000560*
000570 01  CNT-AREA.
000580     03  IN01-CNT                   PIC 9(007) VALUE ZERO.
000590     03  OT01-CNT                   PIC 9(007) VALUE ZERO.
000600     03  OT02-CNT                   PIC 9(007) VALUE ZERO.
000610     03  ERR-CNT                    PIC 9(007) VALUE ZERO.
000620*
000630 01  WORK-AREA.
000640     03  WK-TRAN-NO                 PIC X(015) VALUE SPACE.
000650     03  WK-AMOUNT                  PIC S9(013)V99 VALUE ZERO.
000660     03  WK-ERR-MSG                 PIC X(200) VALUE SPACE.
000670*
000680 01  LIMIT-AREA.
000690     03  MAX-AMOUNT                 PIC S9(013)V99 VALUE 9999999999999.
000700     03  MIN-AMOUNT                 PIC S9(013)V99 VALUE 1.
000710*
000720******************************************************************
000730 LINKAGE                 SECTION.
000740******************************************************************
000750 COPY  SMPC002  REPLACING  ==(*)==  BY  ==LNK==.
000760*
000770*****************************************************************
000780 PROCEDURE               DIVISION  USING  LNK-PARA.
000790*****************************************************************
000800*
000810 0-O-PGMCONTROL          SECTION.
000820*
000830     PERFORM     1-0-INIT-PROG.
000840     PERFORM     2-0-MAIN-PROG
000850            UNTIL       END-FLAG     =    "END".
000860     PERFORM     3-0-FINL-PROG.
000870*
000880     PERFORM     PROGRAM-STATUS-CHECK--CV.
000890*
000900     STOP        RUN.
000910*
000920*****************************************************************
000930*
000940 1-0-INIT-PROG           SECTION.
000950*
000960*****************************************************************
000970 1-0-START.
000980     DISPLAY  "*** SMPM002 PROGRAM START ***" UPON CONSOLE.
000990     OPEN    INPUT                  IN01-FILE
001000             OUTPUT                 OT01-FILE
001010                                    OT02-FILE.
001020*
001030*    Khoi tao vung lam viec
001040     INITIALIZE                     WORK-AREA
001050                                    CNT-AREA.
001060*
001070*    Doc ban ghi dau tien
001080     PERFORM                        1-1-IN01-READ.
001090     IF      END-FLAG  =  "END"
001100             DISPLAY   "??? IN01-FILE GD NOTHING ???"
001110                     UPON CONSOLE
001120     ELSE
001130             CONTINUE
001140     END-IF.
001150*
001160 1-0-EXIT.
001170     EXIT.
001180*
001190*****************************************************************
001200*
001210 2-0-MAIN-PROG           SECTION.
001220*
001230*****************************************************************
001240 2-0-START.
001250*
001260     MOVE  SPACE                TO  ERR-FLAG.
001270*
001280*    Kiem tra so giao dich
001290     IF  IN01-TRAN-NO  =  SPACE
001300         DISPLAY  "*** SMPM002 TRAN-NO SPACE ***" UPON CONSOLE
001310         MOVE  "TRAN-NO SPACE ERROR"  TO  WK-ERR-MSG
001320         MOVE  "Y"                    TO  ERR-FLAG
001330     END-IF.
001340*
001350*    Kiem tra so tien
001360     IF  IN01-TRAN-AMOUNT  <  MIN-AMOUNT  OR
001370         IN01-TRAN-AMOUNT  >  MAX-AMOUNT
001380         DISPLAY  "*** SMPM002 AMOUNT RANGE ERROR ***" UPON CONSOLE
001390         MOVE  "AMOUNT OUT OF RANGE"  TO  WK-ERR-MSG
001400         MOVE  "Y"                    TO  ERR-FLAG
001410     END-IF.
001420*
001430*    Neu co loi thi ghi ra file loi
001440     IF  ERR-YES
001450         MOVE    IN01-TRAN-NO       TO  OT02-TRAN-NO
001460         MOVE    WK-ERR-MSG         TO  OT02-ERROR-MSG
001470         WRITE   OT02-REC
001480         ADD     1                  TO  OT02-CNT
001490         ADD     1                  TO  ERR-CNT
001500         PERFORM                    1-1-IN01-READ
001510         GO      TO  2-0-EXIT
001520     END-IF.
001530*
001540*    Xu ly chinh: ghi ra file ket qua
001550     MOVE    IN01-TRAN-NO       TO  OT01-TRAN-NO.
001560     MOVE    IN01-TRAN-DATE     TO  OT01-TRAN-DATE.
001570     MOVE    IN01-TRAN-TYPE     TO  OT01-TRAN-TYPE.
001580     MOVE    IN01-TRAN-AMOUNT   TO  OT01-TRAN-AMOUNT.
001590     MOVE    IN01-TRAN-CURRENCY TO  OT01-TRAN-CURRENCY.
001600     MOVE    IN01-TRAN-ACCT-NO  TO  OT01-TRAN-ACCT-NO.
001610     MOVE    IN01-TRAN-BRANCH-CD TO OT01-TRAN-BRANCH-CD.
001620     MOVE    0                  TO  OT01-OT-RESULT-CD.
001630*
001640     WRITE   OT01-REC           FROM    IN01-REC.
001650     ADD     1                  TO      OT01-CNT.
001660*
001670     PERFORM                    1-1-IN01-READ.
001680*
001690 2-0-EXIT.
001700     EXIT.
001710*
001720*****************************************************************
001730*
001740 3-0-FINL-PROG           SECTION.
001750*
001760*****************************************************************
001770 3-0-START.
001780     CLOSE                      IN01-FILE
001790                                OT01-FILE
001800                                OT02-FILE.
001810*
001820     DISPLAY  "*** IN01-CNT   = " IN01-CNT  " ***" UPON CONSOLE.
001830     DISPLAY  "*** OT01-CNT   = " OT01-CNT  " ***" UPON CONSOLE.
001840     DISPLAY  "*** OT02-CNT   = " OT02-CNT  " ***" UPON CONSOLE.
001850     DISPLAY  "*** ERR-CNT    = " ERR-CNT   " ***" UPON CONSOLE.
001860     DISPLAY  "*** SMPM002  NORMAL  END ***"        UPON CONSOLE.
001870*
001880 3-0-EXIT.
001890     EXIT.
001900*
001910*****************************************************************
001920*
001930 1-1-IN01-READ           SECTION.
001940*
001950*****************************************************************
001960 1-1-START.
001970     READ  IN01-FILE
001980           AT  END
001990                  MOVE      "END"       TO    END-FLAG
002000           NOT  AT  END
002010                  ADD       1           TO    IN01-CNT.
002020 1-1-EXIT.
002030     EXIT.
002040*
002050***************************************************************
002060*  Kiem tra trang thai chuong trinh
002070 PROGRAM-STATUS-CHECK--CV       SECTION.
002080***************************************************************
002090 PROGRAM-STRAT.
002100*
002110     IF  PROGRAM-STATUS  =  10  THEN                     *>ICC004
002120       MOVE  0                       TO  PROGRAM-STATUS  *>ICC004
002130     ELSE                                                *>ICC004
002140       CONTINUE                                          *>ICC004
002150     END-IF.                                             *>ICC004
002160*
002170 PROGRAM-EXIT.
002180     EXIT.
