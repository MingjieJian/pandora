      subroutine SHAZAM
     $(NL,NT,INPAIR)
C
C     Rudolf Loeser, 1999 Nov 02
C---- Initializes key-array INC for indexing routines INDXUL & INDXNT.
C
C---- The principal diagonal elements are not used.
C     Super-diagonal elements, transposed, pertain to INDXUL, and are
C     serially numbered.
C     Sub-diagonal elements pertain to INDXNT. They are initialized to
C     NULSIG, and then only those elements that pertain to
C     INPAIR-transitions are numbered serially.
C
C     (This is version 2 of SHAZAM.)
C     !DASH
      save
C     !DASH
      integer I, INPAIR, J, N, NL, NT
C     !COM
C---- INDEX       as of 1990 Nov 26
      integer     MILTI,LIMIT,NULSIG,INC
      parameter   (MILTI=50)
C     (Remember to recompile ADAM and GRUB when changing MILTI.)
      dimension   INC(MILTI,MILTI)
      common      /INDEX/ LIMIT,NULSIG,INC
C     Data and controls for the index-mapping routines.
C     .
C     !DASH
      external HI, BYE
C
C               INPAIR(2,NT)
      dimension INPAIR(2,*)
C
      call HI ('SHAZAM')
C     !BEG
      N = 0
      INC( 1 , 1 ) = NULSIG
      do 101 I = 2,NL
        INC( I , I ) = NULSIG
        do 100 J = 1,(I-1)
          N = N+1
          INC( J , I ) = N
          INC( I , J ) = NULSIG
  100   continue
  101 continue
      if(NT.gt.0) then
        N = 0
        do 102 I = 1,NT
          N = N+1
          INC( INPAIR(1,I) , INPAIR(2,I) ) = N
  102   continue
      end if
C     !END
      call BYE ('SHAZAM')
C
      return
      end
