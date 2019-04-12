      subroutine OMEGA
     $(N,Z,F,FI,A,LABEL,DMP,W)
C
C     Rudolf Loeser, 1997 Oct 21
C---- Computes FI, the cumulative integral of F over Z,
C     for diffusion calculations; also
C     delivers the component integrals in A.
C     (This is version 2 of OMEGA.)
C     !DASH
      save
C     !DASH
      real*8 A, F, FI, W, Z
      integer MO, N
      logical DMP, DUMP
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external POMPOUS, CATRIN, HI, BYE
C
      dimension W(*)
C
C               Z(N), F(N), FI(N), A(N)
      dimension Z(*), F(*), FI(*), A(*)
C
      call HI ('OMEGA')
C     !BEG
      DUMP = DMP.and.(MO.gt.0)
      call POMPOUS (N, Z, F, FI, A, LABEL, DUMP, W)
C
      call CATRIN  (FI, N)
      call CATRIN  (A , N)
C     !END
      call BYE ('OMEGA')
C
      return
      end
