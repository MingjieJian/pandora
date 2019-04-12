      subroutine HOPE
     $(X,IX,W,IW)
C     Rudolf Loeser, 1970 Feb 19
C---- Drives normal restart data writing.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IGD, IN, IS, IVEC, IW, IX, IXLB1, IXPBL, JJ304, JJAIJ,
     $        JJBDI, JJBHM, JJCEI, JJCII, JJCP, JJDIO, JJDLV, JJEP1,
     $        JJEP2, JJFCE, JJFON, JJFVS, JJGVL, JJH2N, JJHEA, JJHND,
     $        JJJBN, JJMRJ, JJMSS, JJNCO, JJNK, JJP, JJPAB, JJPBA,
     $        JJPBG, JJPCE, JJPGB, JJPGS, JJPTO, JJQOU, JJR1W, JJRK,
     $        JJRL, JJRRC, JJT5, JJTCO, JJTDN, JJTE, JJTR, JJV, JJV1,
     $        JJV2, JJV3, JJVAM, JJVBM, JJVCM, JJVDM, JJVEL, JJVHA,
     $        JJVM, JJVPR, JJVR, JJVT, JJWRA, JJXK, JJXNC, JJXND, JJXNE,
     $        JJXNU, JJXRK, JJXRL, JJZ, JJZME, LUMR, LUPR, LURR, MOX,
     $        NO, jummy
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(165),JJVR )
      equivalence (IZOQ( 54),JJT5 )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 86),JJBHM)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 12),JJV  )
      equivalence (IZOQ( 17),JJRK )
      equivalence (IZOQ( 18),JJRL )
      equivalence (IZOQ(105),JJEP1)
      equivalence (IZOQ(  2),JJEP2)
      equivalence (IZOQ(174),JJJBN)
      equivalence (IZOQ(123),JJXK )
      equivalence (IZOQ( 50),JJNK )
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(  5),JJ304)
      equivalence (IZOQ( 83),JJQOU)
      equivalence (IZOQ(119),JJMSS)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 56),JJPGS)
      equivalence (IZOQ( 58),JJPTO)
      equivalence (IZOQ( 49),JJVT )
      equivalence (IZOQ(154),JJFON)
      equivalence (IZOQ(140),JJH2N)
      equivalence (IZOQ(184),JJNCO)
      equivalence (IZOQ(180),JJVAM)
      equivalence (IZOQ(196),JJVBM)
      equivalence (IZOQ(197),JJVCM)
      equivalence (IZOQ(198),JJVDM)
      equivalence (IZOQ(188),JJVHA)
      equivalence (IZOQ(187),JJVPR)
      equivalence (IZOQ(192),JJVEL)
      equivalence (IZOQ(193),JJV1 )
      equivalence (IZOQ(194),JJV2 )
      equivalence (IZOQ(195),JJV3 )
      equivalence (IZOQ(181),JJVM )
      equivalence (IZOQ(214),JJXRK)
      equivalence (IZOQ(215),JJXRL)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 28),JJCP )
      equivalence (IZOQ( 29),JJCII)
      equivalence (IZOQ( 30),JJCEI)
      equivalence (IZOQ( 70),JJRRC)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 48),JJZME)
      equivalence (IZOQ(104),JJPBA)
      equivalence (IZOQ(114),JJPGB)
      equivalence (IZOQ(113),JJPBG)
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ(175),JJGVL)
      equivalence (IZOQ(209),JJDIO)
      equivalence (IZOQ(210),JJDLV)
      equivalence (IZOQ(237),JJTCO)
      equivalence (IZOQ(244),JJWRA)
      equivalence (IZOQ(248),JJFCE)
      equivalence (IZOQ(249),JJPCE)
      equivalence (IZOQ(108),JJR1W)
      equivalence (IZOQ( 62),JJTDN)
      equivalence (IZOQ(155),JJFVS)
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ( 73),JJPAB)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  1),JJMRJ)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 4),LURR )
      equivalence (LUNITS(29),LUMR )
      equivalence (LUNITS(30),LUPR )
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external MAGI, GRUNT, POPIO, SIZZLE, FABRI, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(4)
      equivalence
     $(IN (1),IXLB1 ),(IN( 2),IXPBL ),(IN( 3),IGD   ),(IN( 4),IVEC  )
C
      call HI ('HOPE')
C     !BEG
C     (Get, and allocate, W allotment)
      call MAGI   (IN, IS, MOX, 'HOPE')
C     (Initialize populations buffer)
C
      call POPIO  ('INIT', jummy, W(IXPBL))
C---- Initialize (and open) restart files
      call GRUNT  (LURR, LUMR, LUPR)
C
C---- Write restart files
      call SIZZLE (LURR, LUMR, LUPR, X, W(IXLB1), W(IXPBL), X(JJZ),
     $             X(JJHND), X(JJBHM), X(JJXNE), X(JJRK), X(JJR1W),
     $             X(JJRL), X(JJEP1), X(JJEP2), X(JJNK), X(JJXND),
     $             X(JJBDI), X(JJTDN), X(JJ304), X(JJQOU), X(JJMSS),
     $             X(JJTE), X(JJPGS), X(JJPTO), X(JJVT), X(JJFON),
     $             X(JJFVS), X(JJJBN), X(JJXK), X(JJH2N), X(JJNCO),
     $             X(JJVAM), X(JJVBM), X(JJVCM), X(JJVDM), X(JJVHA),
     $             X(JJVPR), X(JJVEL), X(JJV1), X(JJV2), X(JJV3),
     $             X(JJVM), W(IGD), X(JJT5), X(JJXRK), X(JJXRL),
     $             X(JJHEA), X(JJTR), X(JJAIJ), X(JJCP), X(JJCII),
     $             X(JJCEI), X(JJWRA), X(JJRRC), IX(JJMRJ), X(JJXNU),
     $             X(JJP), X(JJZME), X(JJPAB), X(JJPBA), X(JJV),
     $             X(JJPBG), X(JJPGB), X(JJXNC), X(JJGVL), W(IVEC),
     $             X(JJDIO), X(JJDLV), X(JJVR), X(JJTCO), X(JJFCE),
     $             X(JJPCE))
C---- Print miscellanea
      call FABRI  (NO, X(JJTE), X(JJTR), X(JJZ), W(IVEC))
C
C     (Give back W allotment)
      call WGIVE  (W, 'HOPE')
C     !END
      call BYE ('HOPE')
C
      return
      end
