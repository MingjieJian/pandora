      subroutine GRUB
     $(IN,MUX)
C
C     Rudolf Loeser, 1986 Dec 10
C---- Allocates "Part-1 to Part-2" input shuffling data block.
C     (This is version 2 of GRUB.)
C     !DASH
      save
C     !DASH
      integer IN, MUX
C     !COM
C---- NIVELON     as of 1994 May 31
      integer     KMXBND,KMXWAV
      common      /NIVELON/ KMXBND,KMXWAV
C     Limits for tables of Composite Lines Opacity data.
C     .
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
      dimension IN(*)
C
      call HI ('GRUB')
C     !BEG
      IN( 1) = 1
      IN( 2) = IN( 1)+LIMIT+1
      IN( 3) = IN( 2)+LIMIT
      IN( 4) = IN( 3)+50
      IN( 5) = IN( 4)+KMXBND
      IN( 6) = IN( 5)+KMXBND
      IN( 7) = IN( 6)+KMXBND
      MUX    = IN( 7)+2*LIMIT
C     !END
      call BYE ('GRUB')
C
      return
      end
