      subroutine ALATHI
     $(N,HND,HEND,RHEAB,VBMB,VM)
C
C     Rudolf Loeser, 1998 Mar 30
C---- Prints, for THALIA.
C     !DASH
      save
C     !DASH
      real*8 HEND, HND, RHEAB, VBMB, VM
      integer I, LUEO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               HND(N), HEND(N), RHEAB(N), VBMB(N), VM(N)
      dimension HND(*), HEND(*), RHEAB(*), VBMB(*), VM(*)
C
      call HI ('ALATHI')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100)
  100 format(' ','Final summary of Helium abundance ratio ',
     $           'calculation.'//
     $       ' ',17X,'HND',12X,'HEND',11X,'RHEAB',12X,'VBMB',14X,'VM')
      call LINER (1, LUEO)
C
      write (LUEO,101) (I,HND(I),HEND(I),RHEAB(I),VBMB(I),VM(I),I=1,N)
  101 format(5(' ',I4,1P5E16.8/))
C     !END
      call BYE ('ALATHI')
C
      return
      end
