      subroutine WOBLE
     $(IMG,N,TITLE,CALLER,KILROY,KODE)
C
C     Rudolf Loeser, 2002 Nov 19
C---- Prints an editing notification.
C     !DASH
      save
C     !DASH
      integer IMG, KODE, LUEO, N
      logical KILROY
      character CALLER*(*), TITLE*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, IMGPRNT, HI, BYE
C
C               IMG(N)
      dimension IMG(*)
C
      call HI ('WOBLE')
C     !BEG
      if((KODE.eq.1).or.((KODE.eq.2).and.KILROY)) then
        call MESHED  (CALLER, 3)
        KILROY = .false.
      end if
C
      if((KODE.eq.1).or.(KODE.eq.2)) then
        write (LUEO,100) TITLE
  100   format(' ','EDITH edited: ',A)
        call IMGPRNT (LUEO, IMG, N, 1)
      end if
C
      if((KODE.eq.1).or.((KODE.eq.3).and.(.not.KILROY))) then
        call MASHED  (CALLER)
      end if
C     !END
      call BYE ('WOBLE')
C
      return
      end
