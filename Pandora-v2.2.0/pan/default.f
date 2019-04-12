      subroutine DEFAULT
     $(MODE,X)
C
C     Rudolf Loeser, 1969 Apr 04
C---- Sets up calls to REFAULT.
C     !DASH
      save
C     !DASH
      real*8 X, dummy
      integer INK, JJABD, JJADT, JJAHM, JJAL, JJBXI, JJCOL, JJCQA,
     $        JJCQT, JJDDR, JJEQT, JJFIN, JJFNH, JJFRR, JJGMZ, JJHNF,
     $        JJHNV, JJLDT, JJLHM, JJLMD, JJLMM, JJMLC, JJMUF, JJTER,
     $        JJTS, JJVNH, JJWSL, JJXDR, JJXIN, JJXIS, JJXMU, JJZGM, JM,
     $        KBX, KS, LDU, LF, LG, M, MHM, MODE, MQT, MRR, NCB, NCL,
     $        NCQ, NDR, NDT, NFH, NFL, NGM, NLN, NTE, NVH, jummy
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 3),M  )
      equivalence (JZQ(36),KS )
      equivalence (JZQ(19),LF )
      equivalence (JZQ(15),MRR)
      equivalence (JZQ(20),NTE)
      equivalence (JZQ(22),MHM)
      equivalence (JZQ(24),JM )
      equivalence (JZQ(26),LDU)
      equivalence (JZQ(21),NDT)
      equivalence (JZQ(39),MQT)
      equivalence (JZQ(41),NDR)
      equivalence (JZQ(34),LG )
      equivalence (JZQ(51),NCL)
      equivalence (JZQ(54),NVH)
      equivalence (JZQ(55),NCB)
      equivalence (JZQ(53),NCQ)
      equivalence (JZQ(11),KBX)
      equivalence (JZQ(35),INK)
      equivalence (JZQ(52),NLN)
      equivalence (JZQ(31),NFH)
      equivalence (JZQ(16),NFL)
      equivalence (JZQ(58),NGM)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(135),JJFRR)
      equivalence (IZOQ( 22),JJTS )
      equivalence (IZOQ(146),JJXIS)
      equivalence (IZOQ( 79),JJTER)
      equivalence (IZOQ( 84),JJLHM)
      equivalence (IZOQ( 85),JJAHM)
      equivalence (IZOQ( 93),JJLMM)
      equivalence (IZOQ( 94),JJMLC)
      equivalence (IZOQ( 96),JJLMD)
      equivalence (IZOQ( 90),JJLDT)
      equivalence (IZOQ( 91),JJADT)
      equivalence (IZOQ( 92),JJABD)
      equivalence (IZOQ(  1),JJEQT)
      equivalence (IZOQ( 34),JJAL )
      equivalence (IZOQ( 46),JJXDR)
      equivalence (IZOQ( 47),JJDDR)
      equivalence (IZOQ( 15),JJFIN)
      equivalence (IZOQ(130),JJXMU)
      equivalence (IZOQ(115),JJMUF)
      equivalence (IZOQ(183),JJCOL)
      equivalence (IZOQ( 41),JJVNH)
      equivalence (IZOQ(206),JJHNV)
      equivalence (IZOQ(232),JJCQT)
      equivalence (IZOQ(233),JJCQA)
      equivalence (IZOQ(254),JJBXI)
      equivalence (IZOQ( 10),JJHNF)
      equivalence (IZOQ( 21),JJFNH)
      equivalence (IZOQ( 14),JJXIN)
      equivalence (IZOQ(259),JJWSL)
      equivalence (IZOQ(230),JJZGM)
      equivalence (IZOQ(263),JJGMZ)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external REFAULT, HALT, HI, BYE
C
      dimension X(*)
C
      call HI ('DEFAULT')
C     !BEG
      if(MODE.eq.1) then
        call REFAULT (MODE    ,M       ,KS      ,LF      ,MRR     ,
     $                NTE     ,MHM     ,JM      ,LDU     ,NDT     ,
     $                MQT     ,NDR     ,LG      ,NCL     ,NVH     ,
     $                NCQ     ,KBX     ,NFH     ,INK     ,NLN     ,
     $                NGM     ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   )
        NFL = 2*NLN-1
      else if(MODE.eq.2) then
        call REFAULT (MODE    ,M       ,KS      ,jummy   ,MRR     ,
     $                NTE     ,MHM     ,JM      ,LDU     ,NDT     ,
     $                MQT     ,NDR     ,LG      ,NCL     ,NVH     ,
     $                NCQ     ,KBX     ,NFH     ,INK     ,NLN     ,
     $                NGM     ,X(JJFRR),X(JJTS) ,X(JJXIS),dummy   ,
     $                X(JJTER),X(JJLHM),X(JJAHM),X(JJLMM),X(JJMLC),
     $                X(JJLMD),X(JJLDT),X(JJADT),X(JJABD),X(JJEQT),
     $                X(JJAL) ,X(JJXDR),X(JJDDR),X(JJXMU),X(JJCOL),
     $                X(JJVNH),X(JJHNV),X(JJCQT),X(JJCQA),X(JJBXI),
     $                X(JJHNF),X(JJFNH),X(JJXIN),X(JJFIN),X(JJWSL),
     $                X(JJZGM),X(JJGMZ))
      else if(MODE.eq.3) then
        call REFAULT (MODE    ,jummy   ,jummy   ,LF      ,jummy   ,
     $                jummy   ,jummy   ,jummy   ,jummy   ,jummy   ,
     $                jummy   ,jummy   ,jummy   ,jummy   ,jummy   ,
     $                jummy   ,jummy   ,jummy   ,jummy   ,jummy   ,
     $                jummy   ,dummy   ,dummy   ,dummy   ,X(JJMUF),
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   ,dummy   ,dummy   ,dummy   ,
     $                dummy   ,dummy   )
      else
        write (MSSLIN(1),100) MODE
  100   format('MODE =',I12,', which is not 1, 2, or 3.')
        call HALT    ('DEFAULT', 1)
      end if
C     !END
      call BYE ('DEFAULT')
C
      return
      end
