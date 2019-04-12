      subroutine BEAT
     $(X,B,PN,FN,EPN,EFN,BSN,SW,KNT)
C
C     Rudolf Loeser, 1984 Oct 23
C---- Computes one or more versions of the
C     Line Source Function calculation auxiliary terms EP, EF and BS.
C     !DASH
      save
C     !DASH
      real*8 B, BSN, EFN, EPN, FN, PN, X
      integer J, JJAIJ, JJBAT, KNT, N, NL
      logical SW
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 39),JJBAT)
C     !DASH
      external CITRON, HI, BYE
C
      dimension X(*)
C
C               SW(KNT), FN(N,KNT), EPN(N,KNT), EFN(N,KNT), BSN(N,KNT),
      dimension SW(*),   FN(N,*),   EPN(N,*),   EFN(N,*),   BSN(N,*),
C
C               PN(N,KNT), B(N)
     $          PN(N,*),   B(*)
C
      call HI ('BEAT')
C     !BEG
      do 100 J = 1,KNT
        if(SW(J)) then
          call CITRON (N, NL, X(JJAIJ), X(JJBAT), PN(1,J), FN(1,J), B,
     $                 EPN(1,J), EFN(1,J), BSN(1,J))
        end if
  100 continue
C     !END
      call BYE ('BEAT')
C
      return
      end
