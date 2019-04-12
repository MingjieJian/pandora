      subroutine CAMPION
     $(N,Z,FRR,S,Y,X)
C
C     Rudolf Loeser, 1991 Oct 11
C---- Sets up a table of X, distances along disk rays.
C     S and Y are scratch storage.
C     (This is version 2 of CAMPION.)
C     !DASH
      save
C     !DASH
      real*8 FR, FRR, H, R1N, S, X, Y, Z, ZERO, ZN
      integer I, IPEX, LUEO, N
      logical DUMP
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
      equivalence (RZQ( 23),R1N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external CATRIN, MESHED, MASHED, HI, BYE
C
C               Z(N), S(N), Y(N), X(N)
      dimension Z(*), S(*), Y(*), X(*)
C
      call HI ('CAMPION')
C     !BEG
      DUMP = (IPEX.lt.0).or.(IPEX.eq.3)
      FR   = (FRR*R1N)**2
      ZN   = Z(N)
C
      if(DUMP) then
        call MESHED ('CAMPION', 2)
        write (LUEO,100) FR,ZN,R1N
  100   format(' ',1P3E25.17)
      end if
C
      do 102 I = 1,N
        H    = ZN-Z(I)
        S(I) = (R1N+H)**2-FR
        Y(I) = sqrt(S(I))
C
        if(DUMP) then
          write (LUEO,101) I,Z(I),H,S(I),Y(I)
  101     format(' ',I5,1P2E16.8,2E25.17)
        end if
  102 continue
      call CATRIN   (Y, N)
C
      X(1) = ZERO
      do 103 I = 2,N
        X(I) = Y(1)-Y(I)
  103 continue
C
      if(DUMP) then
        call MASHED ('CAMPION')
      end if
C     !END
      call BYE ('CAMPION')
C
      return
      end
