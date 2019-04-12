      subroutine ULFIN
     $(MM,LIM,LL,IND,KODE,I,LABEL,DUMP,DMPM)
C
C     Rudolf Loeser, 1983 Mar 01
C---- Dump administrator.
C     (This is version 2 of ULFIN.)
C     !DASH
      save
C     !DASH
      integer I, IND, IPR01, IPR02, KODE, LIM, LL, LUEO, MM
      logical DMPM, DUMP
      character LABEL*100, TYPE*12
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, LINER, HI, BYE
C
      dimension TYPE(3)
C
      data TYPE /' Shell Rays.', ' Disk Rays. ', ' Mu values. '/
C     !EJECT
C
      call HI ('ULFIN')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.3)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1, 2, or 3.')
        call HALT  ('ULFIN', 1)
      end if
C
      write (LABEL,101) MM,LIM,TYPE(KODE),LL,I
  101 format('*****',I3,' of ',I3,A12,5X,'LL=',I3,', I=',I6)
C
      DMPM = DUMP.and.((IND.ge.IPR01).and.(IND.le.IPR02))
C
      if(DMPM) then
        call LINER (3, LUEO)
        write (LUEO,102) IND,LABEL
  102   format(' ','IND=',I6/
     $         ' ',A100)
      end if
C     !END
      call BYE ('ULFIN')
C
      return
      end
