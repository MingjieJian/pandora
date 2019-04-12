      subroutine CLUMP
     $(Z,XVAL,IBEG,IEND,TIT,KODE)
C
C     Rudolf Loeser, 1982 May 05
C---- Attempts to set up log(Z) as a graph ordinate;
C     returns with KODE=1 if successful, =0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 ONE, R, XVAL, Z, ZERO
      integer I, IBEG, IEND, KODE
      character TIT*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  LOGO, HI, BYE
      intrinsic sign, abs
C
C               Z(N), XVAL(N)
      dimension Z(*), XVAL(*)
C
      call HI ('CLUMP')
C     !BEG
      KODE = 0
C---- Adjust limits (if necessary to avoid Z=0)
      if(Z(IBEG).eq.ZERO) then
        IBEG = IBEG+1
      end if
      if(Z(IEND).eq.ZERO) then
        IEND = IEND-1
      end if
      if(IEND.gt.IBEG) then
C----   Make sure all Z's have the same sign
        R = sign(Z(IBEG),ONE)
        do 100 I = (IBEG+1),IEND
          if((sign(Z(I),ONE).ne.R).or.(Z(I).eq.ZERO)) then
            goto 101
          end if
  100   continue
C----   Set up log(Z)
        call LOGO (Z(IBEG), (IEND-IBEG+1), 0, ZERO, XVAL)
        KODE = 1
        TIT  = 'Log(Z)'
      end if
  101 continue
C     !END
      call BYE ('CLUMP')
C
      return
      end
