      subroutine DASUNI
     $(OPAC,XDSK,TOPT,TDSK,KODE,IMG,W)
C
C     Rudolf Loeser, 1981 Nov 05
C---- Computes Disk Ray Optical Depths.
C     !DASH
      save
C     !DASH
      real*8 OPAC, TDSK, W, XDSK
      integer IMG, J, KODE, MRR, N
      logical TOPT
      character LAB*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
C     !DASH
      external DIDO, HI, BYE
C
      dimension W(*)
C
C               XDSK(N,MRR), TDSK(N,MRR), OPAC(N), IMG(N)
      dimension XDSK(N,*),   TDSK(N,*),   OPAC(*), IMG(*)
C
      call HI ('DASUNI')
C     !BEG
      do 101 J = 1,MRR
C
        write (LAB,100) J
  100   format('Disk ',I3)
C
        call DIDO (XDSK(1,J),OPAC,N,TDSK(1,J),KODE,TOPT,LAB,IMG,W)
        if(KODE.eq.0) then
          goto 102
        end if
C
  101 continue
C
  102 continue
C     !END
      call BYE ('DASUNI')
C
      return
      end
