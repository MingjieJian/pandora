      subroutine IMPALA
     $(M,KBNDS,KINOUT,DEL,F,G,R,S,A,B,C,D,E,Y,CHK,W,IW)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Sets up and solves the five-diagonal diffusion equations for y.
C     (This is version 2 of IMPALA.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, DEL, E, F, G, R, S, W, Y
      integer IFD, IN, IS, IW, IWS, IX, IXL, JN, JNDX, KBINN, KBNDS,
     $        KBOUT, KINOUT, M, MOX, MUX
C     !DASH
      external LIMA, MONGREL, FIVEDAG, LOBBY, WGIVE, ARRDIV, IGIVE,
     $         MALT, NARA, HI, BYE
C
      dimension W(*), IW(*)
C
C               J = N+MXTAP
C
C               F(J), G(J), R(J), S(J), A(J), B(J), C(J), DEL(J), E(J),
      dimension F(*), G(*), R(*), S(*), A(*), B(*), C(*), DEL(*), E(*),
C
C               Y(J), D(J), CHK(J)
     $          Y(*), D(*), CHK(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IX    ),(IN( 2),IXL   ),(IN( 3),IFD   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),JNDX  )
C
      call HI ('IMPALA')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LIMA    (IN, IS,  MOX, 'IMPALA', M)
      call NARA    (JN, IWS, MUX, 'IMPALA', M)
C
C---- Set up boundary conditions
      call MONGREL (KBNDS, KINOUT, KBINN, KBOUT)
C---- Compute A, B, C, D, E
      call ARRDIV  (F, DEL, W(IFD), M)
      call MALT    (M, DEL, W(IFD), G, R, KBINN, KBOUT, A, B, C, D, E)
C---- Solve the system
      call FIVEDAG (A, B, C, D, E, S, M, Y, W(IX), W(IXL), IW(JNDX))
      call LOBBY   (A, B, C, D, E, Y, S, CHK, M)
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'IMPALA')
      call IGIVE   (IW, 'IMPALA')
C     !END
      call BYE ('IMPALA')
C
      return
      end
