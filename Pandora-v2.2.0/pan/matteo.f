      subroutine MATTEO
     $(P,MRJ,LRJ,LZA,IBNE,INLP)
C
C     Rudolf Loeser, 1995 Jul 27
C---- Moves integer arrays into block P (for shuffling to Part 2), and
C     saves P.
C     (This is version 2 of MATTEO.)
C     !DASH
      save
C     !DASH
      real*8 P
      integer IBNE, IIIBNE, IIILR, IIILZA, IIIMR, IIINLP, INLP, LRJ,
     $        LZA, MRJ
C     !COM
C---- INDEX       as of 1990 Nov 26
      integer     MILTI,LIMIT,NULSIG,INC
      parameter   (MILTI=50)
C     (Remember to recompile ADAM and GRUB when changing MILTI.)
      dimension   INC(MILTI,MILTI)
      common      /INDEX/ LIMIT,NULSIG,INC
C     Data and controls for the index-mapping routines.
C     .
C---- NIVELON     as of 1994 May 31
      integer     KMXBND,KMXWAV
      common      /NIVELON/ KMXBND,KMXWAV
C     Limits for tables of Composite Lines Opacity data.
C     .
C---- BERTH       as of 1990 Nov 20
      integer     LSHF,LADR,ISHF
      dimension   ISHF(7)
      common      /BERTH/ LSHF,LADR,ISHF
C     "Part-1 to Part-2" input shuffling data block.
C     (Allocated by GRUB.)
      equivalence (ISHF( 1),IIIMR )
      equivalence (ISHF( 2),IIILR )
      equivalence (ISHF( 3),IIILZA)
      equivalence (ISHF( 6),IIIBNE)
      equivalence (ISHF( 7),IIINLP)
C     !DASH
      external CONTID, CEBU, HI, BYE
C
C               MRJ(LIMIT+1), LRJ(LIMIT), INLP(2*LIMIT), IBNE(KMXBND),
      dimension MRJ(*),       LRJ(*),     INLP(*),       IBNE(*),
C
C               P(LSHF), LZA(50)
     $          P(*),    LZA(*)
C     !EJECT
C
      call HI ('MATTEO')
C     !BEG
      call CONTID (MRJ ,1,LIMIT+1,P(IIIMR ),1,LIMIT+1)
      call CONTID (LRJ ,1,LIMIT  ,P(IIILR ),1,LIMIT  )
      call CONTID (LZA ,1,50     ,P(IIILZA),1,50     )
      call CONTID (IBNE,1,KMXBND ,P(IIIBNE),1,KMXBND )
      call CONTID (INLP,1,2*LIMIT,P(IIINLP),1,2*LIMIT)
C
      call CEBU   (P,LSHF,LADR)
C     !END
      call BYE ('MATTEO')
C
      return
      end
