      subroutine GAG
     $(NL,XM,D,DI,XS,TITLE,W,IW)
C
C     Rudolf Loeser, 1968 Jan 22
C---- GAG computes a series of determinants.
C
C     D = determinant of XM of size (NL-1 x NL-1)
C
C     DI(J) = determinant of XM(J), where XM(J) is a matrix of size
C     (NL-2 x NL-2) obtained from XM by omitting row 1 and column J.
C     !DASH
      save
C     !DASH
      real*8 D, DI, W, XM, XS, ZERO
      integer IW, K, KODE, M, N, NL, NN, NO
      character TITLE*50
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external MOVE1, ROTOR, STRIKE, MAGOG, HI, BYE
C
      dimension W(*), IW(*)
C
C               XM(NL-1,NL-1), XS(NL-1,NL-1), DI(NL-1)
      dimension XM(*),         XS(*),         DI(*)
C
      call HI ('GAG')
C     !BEG
      if(NL.ge.3) then
        N  = NL-1
        NN = N*N
        K  = N-1
C
        write (TITLE(47:50),100) 0
  100   format(I4)
C
        call MOVE1       (XM, NN, XS)
        call ROTOR       (XS, N, TITLE, NO, W, IW, D,     KODE)
        if(KODE.ne.1) then
          call MAGOG     (KODE, TITLE)
        end if
C
        if(N.eq.2) then
          DI(1)  = XM(4)
          DI(2)  = XM(2)
C
        else if(N.gt.2) then
C
          do 101 M = 1,N
            write (TITLE(47:50),100) M
C
            call STRIKE  (M, N, XM, XS)
            call ROTOR   (XS, K, TITLE, NO, W, IW, DI(M), KODE)
            if(KODE.ne.1) then
              call MAGOG (KODE, TITLE)
            end if
  101     continue
        end if
      end if
C     !END
      call BYE ('GAG')
C
      return
      end
