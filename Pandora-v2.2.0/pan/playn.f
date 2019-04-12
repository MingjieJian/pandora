      subroutine PLAYN
     $(FL,F,N,LABEL)
C
C     Rudolf Loeser, 1979 Oct 30
C---- Gets antilogs, for "standard" second-order integration.
C     (This is version 2 of PLAYN.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, FL
      integer I, LUEO, N
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external DPRIVE, MESHED, ABORT, HI, BYE
C
C               FL(N), F(N)
      dimension FL(*), F(*)
C
      data CRIT /7.D2/
C
      call HI ('PLAYN')
C     !BEG
      do 101 I = 1,N
C
        if(FL(I).gt.CRIT) then
          call MESHED ('PLAYN',1)
          write (LUEO,100) LABEL
  100     format(' ','Cannot compute antilogs for:'/
     $           ' ',A)
          call DPRIVE (LUEO,FL,N)
          call ABORT
        end if
C
        F(I) = exp(FL(I))
  101 continue
C     !END
      call BYE ('PLAYN')
C
      return
      end
