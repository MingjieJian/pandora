      subroutine PAW
     $(M,I4DEQ,DEL,F,G,R,S,A,B,C,D,Y,CHK,W,IW)
C
C     Rudolf Loeser, 1997 Aug 27
C---- Sets up and solves the four-diagonal diffusion equations for y.
C     (This is version 2 of PAW.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, DEL, F, G, R, S, W, Y
      integer I4DEQ, IFN, IN, IS, ISN, IW, IWS, IX, IXL, JN, JNDX, M,
     $        MOX, MUX
      logical FULLMAT
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external SAW, RAW, HALT, CYNON, FOURDAG, IGIVE, CAW, MOLL, WGIVE,
     $         JAW, LAW, MAW, HI, BYE
C
      dimension W(*), IW(*)
C
C               J = N+MXTAP
C
C               F(J), G(J), R(J), S(J), A(J), B(J), C(J), D(J), CHK(J),
      dimension F(*), G(*), R(*), S(*), A(*), B(*), C(*), D(*), CHK(*),
C
C               Y(J), DEL(J)
     $          Y(*), DEL(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IX    ),(IN( 2),IXL   ),(IN( 3),IFN   ),(IN( 4),ISN   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),JNDX  )
C
      data FULLMAT /.false./
C     !EJECT
C
      call HI ('PAW')
C     !BEG
      if((I4DEQ.lt.0).or.(I4DEQ.gt.2)) then
        write (MSSLIN(1),100) I4DEQ
  100   format('I4DEQ =',I12,', which is not 0, 1 or 2.')
        call HALT    ('PAW', 1)
      end if
C
C     (Get, and allocate, W & IW allotments)
      call SAW       (IN, IS,  MOX, 'PAW', M)
      call RAW       (JN, IWS, MUX, 'PAW', M)
C
C---- Compute SN and FN
      call MAW       (S, M, W(ISN))
      call MAW       (F, M, W(IFN))
C---- Compute A, B, C, D
      if(I4DEQ.eq.0) then
C       Original
        call CAW     (M, DEL, F, W(IFN), G, R, A, B, C, D)
      else if(I4DEQ.eq.1) then
C       Method 1
        call JAW     (M, DEL, F,         G, R, A, B, C, D)
      else
C       Method 2
        call LAW     (M, DEL, F,         G, R, A, B, C, D)
      end if
C---- Solve the system
      if(FULLMAT) then
        call CYNON   (A, B, C, D, W(ISN), M, Y, W, IW)
      else
        call FOURDAG (A, B, C, D, W(ISN), M, Y, W(IX), W(IXL), IW(JNDX))
      end if
      call MOLL      (A, B, C, D, Y, W(ISN), CHK, M)
C
C     (Give back W & IW allotments)
      call WGIVE     (W,  'PAW')
      call IGIVE     (IW, 'PAW')
C     !END
      call BYE ('PAW')
C
      return
      end
