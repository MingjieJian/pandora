      subroutine HAVGAN
     $(SUM,N,KNFRM,KONFRM)
C
C     Rudolf Loeser, 1988 Apr 13
C---- Sets up KONFRM, for absorbers/emitters printouts.
C     (This is version 2 of HAVGAN.)
C     !DASH
      save
C     !DASH
      real*8 SUM, T
      integer I, KNFRM, KONFRM, LUEO, N
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external  MESHED, MASHED, HI, BYE
      intrinsic abs
C
C               SUM(N)
      dimension SUM(*)
C
      call HI ('HAVGAN')
C     !BEG
      KONFRM = KNFRM
      if((KONFRM.lt.1).or.(KONFRM.gt.2)) then
        call MESHED   ('HAVGAN', 3)
        write (LUEO,100) KNFRM
  100   format(' ','KONFORM =',I10,' is nosense; changed to = 1.')
        call MASHED   ('HAVGAN')
        KONFRM = 1
      end if
      if(KONFRM.eq.2) then
        T = sqrt(ZZSMALL)
        do 101 I = 1,N
          if(abs(SUM(I)).lt.T) then
            KONFRM = 1
            goto 102
          end if
  101   continue
  102   continue
        if(KONFRM.eq.1) then
          call MESHED ('HAVGAN', 3)
          write (LUEO,103)
  103     format(' ','KONFORM set = 1 because the divisor is ',
     $               'unsuitable.')
          call MASHED ('HAVGAN')
        end if
      end if
C     !END
      call BYE ('HAVGAN')
C
      return
      end
