      subroutine DOUR
     $(TAU,S,RHO,XJBAR)
C
C     Rudolf Loeser, 1984 Nov 20
C---- Saves debug checksums, for BOTTOM.
C     (This is version 2 of DOUR.)
C     !DASH
      save
C     !DASH
      real*8 RHO, S, TAU, XJBAR
      integer MO, N
      character TIT*40
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external CHECKER, MISO, HI, BYE
C
C               TAU(N), S(N), RHO(N), XJBAR(N)
      dimension TAU(*), S(*), RHO(*), XJBAR(*)
C
      data TIT /'---'/
C
      call HI ('DOUR')
C     !BEG
      if(MO.gt.0) then
        call MISO    (TIT(4:28))
C
        TIT(1:3) = 'TAU'
        call CHECKER (TAU  ,1,N,TIT)
C
        TIT(1:3) = '  S'
        call CHECKER (S    ,1,N,TIT)
C
        TIT(1:3) = 'RHO'
        call CHECKER (RHO  ,1,N,TIT)
C
        TIT(1:3) = 'JBR'
        call CHECKER (XJBAR,1,N,TIT)
      end if
C     !END
      call BYE ('DOUR')
C
      return
      end
