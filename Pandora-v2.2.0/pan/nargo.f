      subroutine NARGO
     $(I,K)
C
C     Rudolf Loeser, 2003 Feb 19
C---- Selects a depth index value for H Ly lines background opacity.
C     (This is version 2 of NARGO.)
C     !DASH
      save
C     !DASH
      integer I, K
C     !COM
C---- LYSTER      as of 2006 Nov 01
      integer     NLL,NLLY,ILB,LENLYB
      logical     INIHLL,INDPTH
      parameter   (NLLY=500)
      dimension   ILB(18)
      common      /LYSTER1/ NLL,LENLYB,ILB
      common      /LYSTER2/ INIHLL,INDPTH
C     Hydrogen Lyman lines background absorber paraphernalia
C     !DASH
      external HI, BYE
C
      call HI ('NARGO')
C     !BEG
      if(INDPTH) then
        K = I
      else
        K = 1
      end if
C     !END
      call BYE ('NARGO')
C
      return
      end
