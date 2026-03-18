000010*     SMPM001           TEMPORARY LEVEL=1    DATE=26.03.18
000020*****************************************************************
000030*                                                               *
000040*    CHUC NANG  : TRA CUU THONG TIN KHACH HANG                 *
000050*                 Customer Information Inquiry                  *
000060*                                                               *
000070*    PROGAM-ID :  SMPM001                                       *
000080*                                                               *
000090*    UPDATE HISTORY                                             *
000100*       CREATE  26.03.18                                        *
000110*       UPDATE                                                  *
000120*                                                               *
000130*****************************************************************
000140 IDENTIFICATION          DIVISION.
000150 PROGRAM-ID.             SMPM001.
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
000330     COPY  SMPC001    REPLACING  ==(*)==  BY  ==IN01==.
000340*
000350 FD  OT01-FILE
000360              BLOCK       CONTAINS    0       RECORDS.
000370 01  OT01-REC.
000380     COPY  SMPC001    REPLACING  ==(*)==  BY  ==OT01==.
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
000520*
000530 01  WORK-AREA.
000540     03  WK-CUST-NO                 PIC X(010) VALUE SPACE.
000550     03  WK-MSG                     PIC X(080) VALUE SPACE.
000560*
000570******************************************************************
000580 LINKAGE                 SECTION.
000590******************************************************************
000600 COPY  SMPC001  REPLACING  ==(*)==  BY  ==LNK==.
000610*
000620*****************************************************************
000630 PROCEDURE               DIVISION.
000640*****************************************************************
000650*
000660 0-O-PGMCONTROL          SECTION.
000670*
000680     PERFORM     1-0-INIT-PROG.
000690     PERFORM     2-0-MAIN-PROG
000700            UNTIL       END-FLAG     =    "END".
000710     PERFORM     3-0-FINL-PROG.
000720*
000730     PERFORM     PROGRAM-STATUS-CHECK--CV.
000740*
000750     STOP        RUN.
000760*
000770*****************************************************************
000780*
000790 1-0-INIT-PROG           SECTION.
000800*
000810*****************************************************************
000820 1-0-START.
000830     DISPLAY  "*** SMPM001 PROGRAM START ***" UPON CONSOLE.
000840     OPEN    INPUT                  IN01-FILE
000850             OUTPUT                 OT01-FILE.
000860*
000870*    Khoi tao vung lam viec
000880     INITIALIZE                     WORK-AREA
000890                                    CNT-AREA.
000900*
000910*    Doc ban ghi dau tien
000920     PERFORM                        1-1-IN01-READ.
000930     IF      END-FLAG  =  "END"
000940             DISPLAY   "ABC XYZ"
000950                     UPON CONSOLE
000960     ELSE
000970             CONTINUE
000980     END-IF.
000990*
001000 1-0-EXIT.
001010     EXIT.
001020*
001030*****************************************************************
001040*
001050 2-0-MAIN-PROG           SECTION.
001060*
001070*****************************************************************
001080 2-0-START.
001090*    Kiem tra du lieu dau vao
001100     IF  IN01-CUST-NO  =  SPACE
001110         DISPLAY  "*** SMPM001 CUST-NO SPACE ERROR ***"
001120                  UPON CONSOLE
001130         MOVE  "ERR"            TO  OT01-OT-MSG
001140         ADD   1                TO  ERR-CNT
001150         PERFORM                    1-1-IN01-READ
001160         GO    TO  2-0-EXIT
001170     END-IF.
001180*
001190*    Sao chep du lieu tu dau vao den dau ra
001200     MOVE    IN01-CUST-NO       TO  OT01-CUST-NO.
001210     MOVE    IN01-CUST-NAME-X   TO  OT01-CUST-NAME-X.
001220     MOVE    IN01-CUST-BIRTH    TO  OT01-CUST-BIRTH.
001230     MOVE    IN01-CUST-GENDER   TO  OT01-CUST-GENDER.
001240     MOVE    IN01-CUST-PHONE    TO  OT01-CUST-PHONE.
001250     MOVE    IN01-CUST-EMAIL    TO  OT01-CUST-EMAIL.
001260*
001270*    Ghi ban ghi dau ra
001280     WRITE   OT01-REC           FROM    IN01-REC.
001290     ADD     1                  TO      OT01-CNT.
001300*
001310     PERFORM                    1-1-IN01-READ.
001320*
001330 2-0-EXIT.
001340     EXIT.
001350*
001360*****************************************************************
001370*
001380 3-0-FINL-PROG           SECTION.
001390*
001400*****************************************************************
001410 3-0-START.
001420     CLOSE                      IN01-FILE
001430                                OT01-FILE.
001440*
001450     DISPLAY  "*** IN01-CNT   = " IN01-CNT  " ***" UPON CONSOLE.
001460     DISPLAY  "*** OT01-CNT   = " OT01-CNT  " ***" UPON CONSOLE.
001470     DISPLAY  "*** ERR-CNT    = " ERR-CNT   " ***" UPON CONSOLE.
001480     DISPLAY  "*** SMPM001  NORMAL  END ***"        UPON CONSOLE.
001490*
001500 3-0-EXIT.
001510     EXIT.
001520*
001530*****************************************************************
001540*
001550 1-1-IN01-READ           SECTION.
001560*
001570*****************************************************************
001580 1-1-START.
001590     READ  IN01-FILE
001600           AT  END
001610                  MOVE      "END"       TO    END-FLAG
001620           NOT  AT  END
001630                  ADD       1           TO    IN01-CNT.
001640 1-1-EXIT.
001650     EXIT.
001660*
001670***************************************************************
001680*  Kiem tra trang thai chuong trinh
001690 PROGRAM-STATUS-CHECK--CV       SECTION.
001700***************************************************************
001710 PROGRAM-STRAT.
001720*
001730     IF  PROGRAM-STATUS  =  10  THEN                     *>ICC004
001740       MOVE  0                       TO  PROGRAM-STATUS  *>ICC004
001750     ELSE                                                *>ICC004
001760       CONTINUE                                          *>ICC004
001770     END-IF.                                             *>ICC004
001780*
001790 PROGRAM-EXIT.
001800     EXIT.
