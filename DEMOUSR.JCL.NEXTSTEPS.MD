# DEMOUSRJ.JCL — compte rendu de session et prochaines étapes

**Date :** 2026-08-06
**Repo :** `openlegacy/mainframe-modernization-carddemo` (fork de `aws-samples/aws-mainframe-modernization-carddemo`)
**Branche au moment de la session :** `main`, synchro avec `origin/main`, 46 fichiers modifiés non commités

---

## Ce qui a été demandé

1. Reprendre `pattern-component-catalog.md` et **tout recataloguer**
2. Les **DB2TRAN doivent porter le même nom que le tranid codé dans le programme**
3. Bâtir un **JCL de définitions CICS** pour l'ensemble, sous `GROUP(DEMOUSER)`
4. **Un seul nom de schéma** — d'abord `ALAINL`, corrigé ensuite en `DEMOUSER`
5. Le schéma doit aussi être changé **dans le code**

---

## Ce qui a été livré

### 1. `pattern-component-catalog.md` — recatalogué depuis les sources

L'ancien catalogue était largement faux. Corrections principales :

| Problème | Correction |
|---|---|
| AL00 / AD00 / AA00 portaient les tranids de CC00 (`CC00`, `CA00`, `CM00`, `CU00`…) recopiés | Vraies valeurs extraites de `WS-TRANID` / `LIT-THISTRANID` de chaque programme |
| Colonne `DB2tran` = `DB2TRAN / ALAINL` sur **toutes** les lignes, même sur des programmes sans `EXEC SQL` | Colonne `DB2 yes/no` réelle |
| Colonnes `Tables` et `Files` dupliquées et inventées | Ddnames réellement lus (`EXEC CICS READ/STARTBR`, littéraux résolus) + tables réellement interrogées (`EXEC SQL`) |
| Cibles RPC absentes | Ajoutées (`COACTVWS → COACTVWL`, `COCRDLIU → COCRDLIA`, `COACCSTU → COCUSTMA`, …) |
| `COBIL00C` en double, `COACCNTA` absent, `COACTVWD` / `COACTUPS` sans tranid | Corrigé |

### 2. `app/jcl/DEMOUSRJ.jcl` — nouveau

DFHCSDUP, tout sous `GROUP(DEMOUSER)` :

| Ressource | Nombre |
|---|---|
| LIBRARY | 1 |
| FILE (VSAM) | 8 |
| TDQUEUE | 1 |
| MAPSET | 25 |
| PROGRAM | 84 |
| TRANSACTION | 59 |
| DB2ENTRY | 1 |
| DB2TRAN | 30 |

**Décision de design sur les DB2TRAN :** le `DB2ENTRY(DEMOUSER)` n'a
volontairement **pas** de `TRANSID()`. Résultat : chaque transaction passe par
son propre DB2TRAN, et le nom du DB2TRAN est exactement le tranid du
programme.

```
DEFINE DB2TRAN(ADS4) GROUP(DEMOUSER) ENTRY(DEMOUSER) TRANSID(ADS4)
```

30 DB2TRAN = les 15 tranids AD00 + les 15 tranids AA00. CC00 et AL00 sont du
VSAM pur, ils n'en ont pas.

**Vérifications passées :**
- les 59 tranids du JCL correspondent au caractère près à ceux des sources COBOL
- les 30 DB2TRAN ont bien `nom == TRANSID`
- aucun tranid ni nom de programme en double
- toutes les lignes ≤ colonne 72
- tous les `DEFINE PROGRAM` ont un source correspondant ; seuls les programmes batch (CBACT*, CBTRN*, CBCUS01C, CBSTM03*, CSUTLDTL) sont exclus, à raison

### 3. Schéma `ALAINL` → `DEMOUSER`, dans le JCL et dans le code

JCL — symboles :

```
//   SET SCHEMA=DEMOUSER
//   SET DB2PLAN=DEMOUSER
```

Code — **64 instructions SQL dans 26 programmes** qualifiées `DEMOUSER.` :

| | Programmes | Statements |
|---|---|---|
| Portaient `ALAINL.` | COCRDLID, COCRDSLD, COCRDLIA, COCRDSLA, COTRN2AD, COTRN2CD | 15 |
| N'avaient **aucun** qualifiant | COACTUPD, COACTVWD, COCRDUPD, COUSR00D-03D, COACCNTA, COACTADA, COACTUPA, COACTVWA, COCCARDA, COCRDUPA, COCUSADA, COCUSTMA, COTRANSA, COUSR00A-03A | 49 |

Par table : `DEMOUSER.CARDDAT` ×20, `DEMOUSER.USERSEC` ×16,
`DEMOUSER.CUSTDAT` ×12, `DEMOUSER.ACCTDAT` ×10, `DEMOUSER.CXACAIX` ×3,
`DEMOUSER.TRANSACT` ×3.

Les 49 non qualifiés ont été traités en plus des 6 visés, sinon la moitié des
programmes serait restée dépendante du `QUALIFIER` du bind pendant que
l'autre moitié était écrite en dur.

Vérifié : 0 référence de table non qualifiée restante dans un bloc
`EXEC SQL` ; aucune ligne modifiée ne dépasse la colonne 72 ; fins de ligne
préservées ; le `git diff` ne touche que les lignes SQL.

---

## Anomalies relevées dans le code existant

### `COUSR01D` code `AD01`, pas `ADS1`

[app/cbl-db2/COUSR01D.cbl:37](app/cbl-db2/COUSR01D.cbl#L37) — seul de la série
AD00 à casser la convention `ADS`*n* (les autres sont ADS0/ADS2/ADS3).
`AD01` a été retenu partout (TRANSACTION + DB2TRAN) parce que c'est ce que le
programme envoie sur `RETURN TRANSID`. **À décider :** renommer en `ADS1`
dans le source, et changer le JCL avec.

### `EXTSEC(YES)` sous `NOCOMPAT`

Tes modifs en cours ajoutent `RESSEC(YES) CMDSEC(YES) EXTSEC(YES)` dans
`app/csd/CARDDEMO.CSD` et `app/jcl/CBADMCDJ.jcl`. `EXTSEC` est un attribut
obsolète : avec `PARM='...,NOCOMPAT'` DFHCSDUP le rejette. **CBADMCDJ risque
de planter là-dessus.** `DEMOUSRJ.jcl` n'utilise que `RESSEC(YES) CMDSEC(YES)`.

### Programmes orphelins

- `COACCNTA`, `COCCARDA`, `COTRANSA` — aucun appelant COBOL dans le repo ;
  ce sont des endpoints OpenLegacy appelés de l'extérieur de CICS
- `COCUSADU` (`AASD`) — pas dans `COMEN02U`, atteignable seulement par son
  propre tranid
- `COCRDSEC` / transaction `CDV1` — définis dans l'ancien
  `app/csd/CARDDEMO.CSD` mais **aucun source dans le repo** ; volontairement
  laissés hors de DEMOUSER
- `COTRN2AD → COTRN2BD → COTRN2CD` — flux « add transaction » en 3 écrans
  atteint par ses propres tranids, **pas** câblé dans `COMEN02D`

---

## Prochaines étapes

### 1. Créer la branche avant de commiter

Tu es sur `main` avec 46 fichiers sales. Recommandation retenue : **branche,
pas nouveau repo** — un nouveau repo couperait le lien avec `upstream`
(mises à jour du CardDemo AWS) et l'historique des commits patterns.

```
git switch -c demouser-cics-catalog
```

Deux lots indépendants, séparables en deux commits :

- **Lot A — travail DEMOUSER de cette session :** 26 programmes DB2
  (qualification `DEMOUSER.`), `app/jcl/DEMOUSRJ.jcl` (nouveau),
  `pattern-component-catalog.md` (nouveau), ce fichier
- **Lot B — tes modifs antérieures :** contrôles `CDEMO-USER-ID` dans 16
  programmes, `RESSEC/CMDSEC/EXTSEC` dans `CARDDEMO.CSD` et `CBADMCDJ.jcl`

### 2. Bind du plan avec `QUALIFIER(DEMOUSER)`

Le JCL de compile/bind est de ton côté. Le schéma est maintenant à **trois
endroits qui doivent s'accorder** :

| Endroit | Valeur |
|---|---|
| `SET SCHEMA=` dans DEMOUSRJ.jcl → `AUTHID()` du DB2ENTRY | `DEMOUSER` |
| `QUALIFIER()` du bind du plan | `DEMOUSER` |
| Le SQL dans le COBOL | `DEMOUSER.` |

Comme le SQL est maintenant entièrement qualifié, le `QUALIFIER` du bind ne
détermine plus quel schéma est atteint. Changer de schéma demande désormais
un renommage global dans le COBOL **en plus** du symbole JCL.

### 3. Points à trancher dans `DEMOUSRJ.jcl`

- **`DB2CONN`** — présent mais **commenté**. Une seule peut être installée
  par région ; l'installer écraserait la connexion existante. Décommenter et
  ajuster `DB2ID(DBC1)` seulement si la région n'en a pas déjà une.
- **Les 8 `DEFINE FILE`** — doublons de ceux de `GROUP(CARDDEMO)`, mêmes
  DSN. Volontaire pour que DEMOUSER soit autonome. À commenter si tu
  continues d'installer CARDDEMO pour les données.
- **`ADD GROUP(DEMOUSER) LIST(...)`** — commenté, le nom de la liste de
  démarrage n'était pas connu.
- **`THREADLIMIT(5)`** sur le DB2ENTRY — doit rester sous le `TCBLIMIT` du
  DB2CONN de la région.
- **`AD01`** — voir anomalie ci-dessus.

### 4. Installation

```
CEDA INSTALL GROUP(DEMOUSER)
```

Le JCL contient un `DELETE GROUP(DEMOUSER)` commenté pour les reruns.

---

## Fichiers touchés dans cette session

**Créés**
- `app/jcl/DEMOUSRJ.jcl`
- `pattern-component-catalog.md` (réécrit intégralement)
- `DEMOUSR.JCL.NEXTSTEPS.MD` (ce fichier)

**Modifiés — qualification du schéma uniquement**
- `app/cbl-db2/` : COACTUPD, COACTVWD, COCRDLID, COCRDSLD, COCRDUPD,
  COTRN2AD, COTRN2CD, COUSR00D, COUSR01D, COUSR02D, COUSR03D
- `app/cbl-rpc-db2/` : COACCNTA, COACTADA, COACTUPA, COACTVWA, COCCARDA,
  COCRDLIA, COCRDSLA, COCRDUPA, COCUSADA, COCUSTMA, COTRANSA, COUSR00A,
  COUSR01A, COUSR02A, COUSR03A

**Non touchés** — le JCL de compile/bind (de ton côté), les programmes batch,
`app/csd/CARDDEMO.CSD`, `app/jcl/CBADMCDJ.jcl`.
