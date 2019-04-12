      subroutine CLOTH
     $(LURR,XNK,XND,BDI)
C
C     Rudolf Loeser, 1980 Jan 04
C---- Puts "number density" data into restart file.
C     !DASH
      save
C     !DASH
      real*8 BDI, XND, XNK
      integer LURR, MODE, N, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external BUNT, PANT, HI, BYE
C
C               XND(N,NL), BDI(N,NL), XNK(N)
      dimension XND(*),    BDI(*),    XNK(*)
C
      data MODE /1/
C
      call HI ('CLOTH')
C     !BEG
      call BUNT (LURR,XNK,          'NK')
      call PANT (LURR,XND,N,NL,MODE,'ND')
      call PANT (LURR,BDI,N,NL,MODE,'BD')
C     !END
      call BYE ('CLOTH')
C
      return
      end
