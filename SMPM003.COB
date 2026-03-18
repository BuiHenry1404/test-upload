000010*     SMPM003           TEMPORARY LEVEL=1    DATE=26.03.18
000020*****************************************************************
000030*                                                               *
000040*    CHUC NANG  : KIEM TRA VA CAP NHAT THONG TIN SAN PHAM       *
000050*                 Product Information Validation & Update        *
000060*                                                               *
000070*    PROGAM-ID :  SMPM003                                       *
000080*                                                               *
000090*    UPDATE HISTORY                                             *
000100*       CREATE  26.03.18                                        *
000110*       UPDATE                                                  *
000120*                                                               *
000130*****************************************************************
000140 IDENTIFICATION          DIVISION.
000150 PROGRAM-ID.             SMPM003.
000160*
000170 ENVIRONMENT             DIVISION.
000180 CONFIGURATION           SECTION.
000190 SOURCE-COMPUTER.        FACOM.
000200 OBJECT-COMPUTER.        FACOM.
000210 INPUT-OUTPUT            SECTION.
000220 FILE-CONTROL.
000230     SELECT        IN01-FILE      ASSIGN  TO  IN01.
000240     SELECT        OT01-FILE      ASSIGN  TO  OT01.
000250*
000260*****************************************************************
000270 DATA                    DIVISION.
000280*****************************************************************
000290 FILE                    SECTION.
000300*
000310 FD  IN01-FILE.
000320 01  IN01-REC.
000330     COPY  SMPC003    REPLACING  ==(*)==  BY  ==IN01==.
000340*
000350 FD  OT01-FILE
000360              BLOCK       CONTAINS    0       RECORDS.
000370 01  OT01-REC.
000380     COPY  SMPC003    REPLACING  ==(*)==  BY  ==OT01==.
000390*
000400*****************************************************************
000410 WORKING-STORAGE         SECTION.
000420*****************************************************************
000430*
000440 01  FLAG-AREA.
000450     03  END-FLAG                   PIC X(003) VALUE SPACE.
000460         88  WK-END                 VALUE "END".
000470*
000480 01  CNT-AREA.
000490     03  IN01-CNT                   PIC 9(007) VALUE ZERO.
000500     03  OT01-CNT                   PIC 9(007) VALUE ZERO.
000510     03  ERR-CNT                    PIC 9(007) VALUE ZERO.
000520     03  UPD-CNT                    PIC 9(007) VALUE ZERO.
000530*
000540 01  WORK-AREA.
000550     03  WK-PROD-CD                 PIC X(010) VALUE SPACE.
000560     03  WK-TOTAL-STOCK             PIC 9(007) VALUE ZERO.
000570     03  WK-IDX                     PIC 9(002) VALUE ZERO.
000580*
000590 01  CONST-AREA.
000600     03  MAX-PROD-STOCK             PIC 9(007) VALUE 9999999.
000610*
000620******************************************************************
000630 LINKAGE                 SECTION.
000640******************************************************************
000650 COPY  SMPC003  REPLACING  ==(*)==  BY  ==LNK==.
000660*
000670*****************************************************************
000680 PROCEDURE               DIVISION  USING  LNK-PARA.
000690*****************************************************************
000700*
000710 0-O-PGMCONTROL          SECTION.
000720*
000730     PERFORM     1-0-INIT-PROG.
000740     PERFORM     2-0-MAIN-PROG
000750            UNTIL       END-FLAG     =    "END".
000760     PERFORM     3-0-FINL-PROG.
000770*
000780     PERFORM     PROGRAM-STATUS-CHECK--CV.
000790*
000800     STOP        RUN.
000810*
000820*****************************************************************
000830*
000840 1-0-INIT-PROG           SECTION.
000850*
000860*****************************************************************
000870 1-0-START.
000880     DISPLAY  "*** SMPM003 PROGRAM START ***" UPON CONSOLE.
000890     OPEN    INPUT                  IN01-FILE
000900             OUTPUT                 OT01-FILE.
000910*
000920*    Khoi tao vung lam viec
000930     INITIALIZE                     WORK-AREA
000940                                    CNT-AREA.
000950*
000960*    Doc ban ghi dau tien
000970     PERFORM                        1-1-IN01-READ.
000980     IF      END-FLAG  =  "END"
000990             DISPLAY   "??? IN01-FILE DATA NOTHING ???"
001000                     UPON CONSOLE
001010     ELSE
001020             CONTINUE
001030     END-IF.
001040*
001050 1-0-EXIT.
001060     EXIT.
001070*
001080*****************************************************************
001090*
001100 2-0-MAIN-PROG           SECTION.
001110*
001120*****************************************************************
001130 2-0-START.
001140*
001150*    Kiem tra ma san pham
001160     IF  IN01-PROD-CD  =  SPACE
001170         DISPLAY  "*** SMPM003 PROD-CD SPACE ERROR ***"
001180                  UPON CONSOLE
001190         ADD   1                TO  ERR-CNT
001200         PERFORM                    1-1-IN01-READ
001210         GO    TO  2-0-EXIT
001220     END-IF.
001230*
001240*    Kiem tra gia ban
001250     IF  IN01-PROD-PRICE  <=  ZERO
001260         DISPLAY  "*** SMPM003 PRICE INVALID ***"
001270                  UPON CONSOLE
001280         ADD   1                TO  ERR-CNT
001290         PERFORM                    1-1-IN01-READ
001300         GO    TO  2-0-EXIT
001310     END-IF.
001320*
001330*    Tinh tong ton kho tu cac kho con
001340     MOVE  ZERO                 TO  WK-TOTAL-STOCK.
001350     PERFORM  VARYING  WK-IDX  FROM  1  BY  1  UNTIL  (WK-IDX  >  5)
001360         ADD   IN01-PROD-STOCK-QTY-A(WK-IDX)  TO  WK-TOTAL-STOCK
001370     END-PERFORM.
001380*
001390*    Kiem tra tong ton kho so voi gia tri luu
001400     IF  WK-TOTAL-STOCK  NOT  =  IN01-PROD-STOCK-QTY-T
001410         MOVE    WK-TOTAL-STOCK     TO  IN01-PROD-STOCK-QTY-T
001420         ADD     1                  TO  UPD-CNT
001430     END-IF.
001440*
001450*    Set co hieu luc
001460     IF  WK-TOTAL-STOCK  >  ZERO
001470         SET  IN01-PROD-VALID      TO  TRUE
001480     ELSE
001490         SET  IN01-PROD-INVALID    TO  TRUE
001500     END-IF.
001510*
001520*    Ghi ban ghi dau ra
001530     MOVE    IN01-PROD-CD           TO  OT01-PROD-CD.
001540     MOVE    IN01-PROD-NAME-X       TO  OT01-PROD-NAME-X.
001550     MOVE    IN01-PROD-CATEGORY     TO  OT01-PROD-CATEGORY.
001560     MOVE    IN01-PROD-PRICE        TO  OT01-PROD-PRICE.
001570     MOVE    IN01-PROD-UNIT         TO  OT01-PROD-UNIT.
001580     MOVE    IN01-PROD-STOCK-QTY    TO  OT01-PROD-STOCK-QTY.
001590     MOVE    IN01-PROD-VALID-FLG    TO  OT01-PROD-VALID-FLG.
001600     MOVE    0                      TO  OT01-OT-RTN-CD.
001610*
001620     WRITE   OT01-REC           FROM    IN01-REC.
001630     ADD     1                  TO      OT01-CNT.
001640*
001650     PERFORM                    1-1-IN01-READ.
001660*
001670 2-0-EXIT.
001680     EXIT.
001690*
001700*****************************************************************
001710*
001720 3-0-FINL-PROG           SECTION.
001730*
001740*****************************************************************
001750 3-0-START.
001760     CLOSE                      IN01-FILE
001770                                OT01-FILE.
001780*
001790     DISPLAY  "*** IN01-CNT   = " IN01-CNT  " ***" UPON CONSOLE.
001800     DISPLAY  "*** OT01-CNT   = " OT01-CNT  " ***" UPON CONSOLE.
001810     DISPLAY  "*** UPD-CNT    = " UPD-CNT   " ***" UPON CONSOLE.
001820     DISPLAY  "*** ERR-CNT    = " ERR-CNT   " ***" UPON CONSOLE.
001830     DISPLAY  "*** SMPM003  NORMAL  END ***"        UPON CONSOLE.
001840*
001850 3-0-EXIT.
001860     EXIT.
001870*
001880*****************************************************************
001890*
001900 1-1-IN01-READ           SECTION.
001910*
001920*****************************************************************
001930 1-1-START.
001940     READ  IN01-FILE
001950           AT  END
001960                  MOVE      "END"       TO    END-FLAG
001970           NOT  AT  END
001980                  ADD       1           TO    IN01-CNT.
001990 1-1-EXIT.
002000     EXIT.
002010*
002020***************************************************************
002030*  Kiem tra trang thai chuong trinh
002040 PROGRAM-STATUS-CHECK--CV       SECTION.
002050***************************************************************
002060 PROGRAM-STRAT.
002070*
002080     IF  PROGRAM-STATUS  =  10  THEN                     *>ICC004
002090       MOVE  0                       TO  PROGRAM-STATUS  *>ICC004
002100     ELSE                                                *>ICC004
002110       CONTINUE                                          *>ICC004
002120     END-IF.                                             *>ICC004
002130*
002140 PROGRAM-EXIT.
002150     EXIT.
