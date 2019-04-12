      subroutine CONRAD
     $(KTRU,KJTAUK,KJSCON,KJOPAC,KJCNXP,KJFD,KJKONT)
C
C     Rudolf Loeser, 1992 Apr 17
C---- Sets up Continuum Data Block indices for one of two cases:
C     KTRU = 1: "line-free" background (continuum);
C     KTRU = 0: regular background.
C     !DASH
      save
C     !DASH
      integer KJCNXP, KJFD, KJKONT, KJOPAC, KJSCON, KJTAUK, KKCNXP,
     $        KKFD, KKKNTT, KKKONT, KKOPAC, KKSCON, KKTAUK, KKTFD,
     $        KKTNXP, KKTOPA, KKTSCN, KKTTAU, KTRU
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(12),KKTAUK)
      equivalence (KKK(49),KKTTAU)
      equivalence (KKK(14),KKSCON)
      equivalence (KKK(50),KKTSCN)
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK(51),KKTOPA)
      equivalence (KKK(11),KKCNXP)
      equivalence (KKK(52),KKTNXP)
      equivalence (KKK( 3),KKKONT)
      equivalence (KKK(56),KKKNTT)
      equivalence (KKK(17),KKFD  )
      equivalence (KKK(53),KKTFD )
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('CONRAD')
C     !BEG
      if(KTRU.eq.1) then
        KJTAUK = KKTTAU
        KJSCON = KKTSCN
        KJOPAC = KKTOPA
        KJCNXP = KKTNXP
        KJFD   = KKTFD
        KJKONT = KKKNTT
C
      else
C
        KJTAUK = KKTAUK
        KJSCON = KKSCON
        KJOPAC = KKOPAC
        KJCNXP = KKCNXP
        KJFD   = KKFD
        KJKONT = KKKONT
      end if
C     !END
      call BYE ('CONRAD')
C
      return
      end
