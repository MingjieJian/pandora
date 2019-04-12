      subroutine CANIS
     $(N,TE,INCEI)
C
C     Rudolf Loeser, 2006 Apr 12
C---- Sets up default value of INCEI.
C     (This is version 2 of CANIS.)
C     !DASH
      save
C     !DASH
      real*8 ONE, TE, TER, TMIN
      integer I, IMIN, INCEI, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic abs, sign
C
C               TE(N)
      dimension TE(*)
C
      data TER /8.D3/
C
      call HI ('CANIS')
C     !BEG
      if((INCEI.lt.1).or.(INCEI.gt.N)) then
        IMIN = 1
        TMIN = TE(1)
        do 100 I = 2,N
          if(TE(I).lt.TMIN) then
            IMIN = I
            TMIN = TE(I)
          end if
          if(sign(ONE,(TE(I-1)-TER)).ne.sign(ONE,(TE(I)-TER))) then
            goto 101
          end if
  100   continue
        INCEI = IMIN
        goto 102
  101   continue
        if(abs(TE(I-1)-TER).lt.abs(TE(I)-TER)) then
          INCEI = I-1
        else
          INCEI = I
        end if
  102   continue
      end if
C     !END
      call BYE ('CANIS')
C
      return
      end
