      subroutine DERNAY
     $(LUMR,Z,TE,XNE,HND,V,VT,BDHM,VM)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts data for Juan Fontenla into restart file.
C     !DASH
      save
C     !DASH
      real*8 BDHM, HND, TE, V, VM, VT, XNE, Z
      integer I, LUMR, N
      character DATE*11, QMODL*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  3),QMODL)
C
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
C     !DASH
C     !EJECT
      external GET_DATE, YARDEN, HI, BYE
C
C               BDHM(N), XNE(N), HND(N), VM(N), VT(N), TE(N), Z(N), V(N)
      dimension BDHM(*), XNE(*), HND(*), VM(*), VT(*), TE(*), Z(*), V(*)
C
      call HI ('DERNAY')
C     !BEG
      call YARDEN   (LUMR, 1, 'FONTENLA')
C
      call GET_DATE (DATE)
      write (LUMR,100) QMODL,DATE,VERSION
  100 format(A8,2X,'model as of ',A11,' (PANDORA program version',
     $       F8.3,')')
C
      write (LUMR,101)
  101 format(14X,'Height',5X,'Temperature',14X,'ne',14X,'nH',14X,'vt',
     $       14X,'vp',9X,'bHminus',8X,'Velocity')
C
      write (LUMR,102) (I,Z(I),TE(I),XNE(I),HND(I),V(I),VT(I),BDHM(I),
     $                    VM(I),I=1,N)
  102 format(I4,1P8E16.8)
C
      call YARDEN   (LUMR, 2, 'FONTENLA')
C     !END
      call BYE ('DERNAY')
C
      return
      end
