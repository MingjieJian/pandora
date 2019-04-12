      subroutine SOTUR
     $(MODE,KASE,KIS,KIL)
C
C     Rudolf Loeser, 2000 Feb 07
C---- Stores (MODE = 1) or retrieves (MODE = 2) tau-reduction data
C     from some weight matrix calculations.
C     !DASH
      save
C     !DASH
      integer KASE, KIL, KIS, MODE
C     !COM
C---- TORUS       as of 2000 Feb 07
      integer     KTRKAS,KTRKIS,KTRKIL
      common      /TORUS/ KTRKAS,KTRKIS,KTRKIL
C     Tau-reduction data from some weight matrix calculations.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, HI, BYE
C
      call HI ('SOTUR')
C     !BEG
      if(MODE.eq.1) then
        KTRKAS = KASE
        KTRKIS = KIS
        KTRKIL = KIL
      else if(MODE.eq.2) then
        KASE = KTRKAS
        KIS  = KTRKIS
        KIL  = KTRKIL
      else
        write (MSSLIN(1),100) MODE
  100   format('MODE =',I12,', which is neither 1 nor 2.')
        call HALT ('SOTUR', 1)
      end if
C     !END
      call BYE ('SOTUR')
C
      return
      end
