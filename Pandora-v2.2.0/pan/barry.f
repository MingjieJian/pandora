      subroutine BARRY
     $(KODE,X,F,N,AVE)
C
C     Rudolf Loeser, 1993 Feb 19
C---- Integrates continuum data.
C     KODE=1 for X = wavenumber, =2 for X = wavelength.
C     !DASH
      save
C     !DASH
      real*8 AVE, DIV, F, ONE, SUM, TWO, X, X1, XL, XR, ZERO
      integer I, KODE, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  HALT, DIVIDE, HI, BYE
C
C               X(N), F(N)
      dimension X(*), F(*)
C     !EJECT
C
      call HI ('BARRY')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is neither 1 nor 2.')
        call HALT       ('BARRY', 1)
      end if
C
      if(N.le.1) then
        AVE = F(1)
C
      else
        SUM = ZERO
        if(KODE.eq.1) then
          X1 = X(1)
        else
          call DIVIDE   (ONE,X(1),X1)
        end if
        XR = X1
C
        do 101 I = 2,N
          XL = XR
          if(KODE.eq.1) then
            XR = X(I)
          else
            call DIVIDE (ONE,X(I),XR)
          end if
          SUM = SUM+(F(I-1)+F(I))*(XR-XL)
  101   continue
C
        DIV = TWO*(XR-X1)
        call DIVIDE     (SUM,DIV,AVE)
      end if
C     !END
      call BYE ('BARRY')
C
      return
      end
