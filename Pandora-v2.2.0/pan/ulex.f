      subroutine ULEX
     $(KODE,IA)
C
C     Rudolf Loeser, 1983 Oct 19
C---- Prints dump header, for FURZE.
C     !DASH
      save
C     !DASH
      integer IA, KODE, LUEO
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HALT, HI, BYE
C
      call HI ('ULEX')
C     !BEG
      call LINER (2, LUEO)
      write (LUEO,100)
  100 format(' ','Intensity for a ray in spherical coordinates.')
C
      if(KODE.eq.1) then
        write (LUEO,101) IA
  101   format(' ','Ray tangent to Shell with radius of',I4,'. depth.')
      else if(KODE.eq.2) then
        write (LUEO,102) IA
  102   format(' ','Ray tangent to Shell corresponding to',I3,
     $             '. fractional radius.')
      else
        write (MSSLIN(1),103) KODE
  103   format('KODE =',I12,', which is neither 1 nor 2.')
        call HALT ('ULEX',1)
      end if
C
      call LINER (1, LUEO)
      write (LUEO,104)
  104 format(' ',17X,'FDX',12X,'EFDX',11X,'PEFDX',14X,'XK',12X,'TERM',
     $           14X,'FI')
C     !END
      call BYE ('ULEX')
C
      return
      end
