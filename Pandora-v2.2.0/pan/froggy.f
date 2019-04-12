      subroutine FROGGY
     $(TITLE,ARRAY,N,LINE,IZERO,NO,IB,KODE,NN)
C
C     Rudolf Loeser, 1990 Dec 31
C---- Special version of FROGG, to deal with arrays whose elements
C     are all equal. Sets uo KODE when IB = 1, and returns KODE for
C     use in subsequent calls to FROGGY.
C
C---- Put IZERO = 0 to print floating zero as blank;
C               = 1                           "0".
C     !DASH
      save
C     !DASH
      real*8 ARRAY, CON, ZERO
      integer IB, IZERO, KODE, N, NN, NO
      logical KNST
      character LINE*120, TITLE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  KONSTD, RIGHT, FROGG, HI, BYE
C
C               ARRAY(N or NN)
      dimension ARRAY(*)
C
      call HI ('FROGGY')
C     !BEG
      if(IB.eq.1) then
        CON = ARRAY(1)
        call KONSTD  (ARRAY,1,NN,CON,KNST)
        if(KNST.and.(CON.ne.ZERO)) then
          call RIGHT (TITLE,LINE(1:40),40)
          write (NO,100) LINE(2:40),CON
  100     format(' ',A39,'     all =',1PE10.3)
          KODE = 1
        else
          KODE = 0
        end if
      end if
C
      if(KODE.eq.0) then
        call FROGG   (TITLE,ARRAY,N,LINE,IZERO,NO)
      end if
C     !END
      call BYE ('FROGGY')
C
      return
      end
