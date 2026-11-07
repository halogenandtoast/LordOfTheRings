module LordOfTheRings.Enemy.Cards.AnnuminasInvaders
  (hostOfAngmar, wargOfCarnDum, orcOfAngmar, angmarWargRider, savageWerewolf, vanguardOfCarnDum) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (exhaustableCharacters)
import LordOfTheRings.Placement (Placement (StagingArea))

newtype AnnuminasInvader = AnnuminasInvader EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

hostOfAngmar, wargOfCarnDum, orcOfAngmar, angmarWargRider, savageWerewolf, vanguardOfCarnDum :: EnemyCard AnnuminasInvader
hostOfAngmar = enemy AnnuminasInvader Cards.hostOfAngmar
wargOfCarnDum = enemy AnnuminasInvader Cards.wargOfCarnDum
orcOfAngmar = enemy AnnuminasInvader Cards.orcOfAngmar
angmarWargRider = enemy AnnuminasInvader Cards.angmarWargRider
savageWerewolf = enemy AnnuminasInvader Cards.savageWerewolf
vanguardOfCarnDum = enemy AnnuminasInvader Cards.vanguardOfCarnDum

instance HasModifiersFor AnnuminasInvader where
  getModifiersFor (AnnuminasInvader a) = do
    when (toCardCode a == toCardCode Cards.hostOfAngmar) $ modifySelf a [ImmuneToPlayerCardEffects]
    when (toCardCode a == toCardCode Cards.vanguardOfCarnDum) do
      modifySelf a [CannotHaveAttachments]
      when (a.placement == StagingArea) $
        getMainQuests >>= traverse_ (\stage -> modified a stage [ProgressPerRoundLimit 5])

instance HasAbilities AnnuminasInvader where
  getAbilities (AnnuminasInvader a)
    | code `elem` map toCardCode [Cards.wargOfCarnDum, Cards.angmarWargRider] = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]
    | code == toCardCode Cards.orcOfAngmar = [mkAbility a 2 $ forced $ EnemyAttacksWindow When a.id]
    | code `elem` map toCardCode [Cards.hostOfAngmar, Cards.savageWerewolf] = [mkAbility a 3 $ forced $ EnemyAttacksAndDestroysCharacterWindow After a.id]
    | otherwise = []
   where
    code = toCardCode a

-- "Either" offers an option only when it can resolve in full (FAQ 1.44).
progressChoice :: (ReverseQueue m, Sourceable source) => source -> Int -> ChooseT m ()
progressChoice source n = whenM ((>= n) <$> getMainQuestProgress) $
  labeledWith "annuminas.removeProgress" [intVar "count" n] $ removeProgressFromMainQuest source n

instance RunMessage AnnuminasInvader where
  runMessage msg e@(AnnuminasInvader a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [pid | Window After (EnemyEngaged eid pid) <- windows, eid == a.id] $ \pid ->
        if toCardCode a == toCardCode Cards.wargOfCarnDum
          then do
            characters <- exhaustableCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
            chooseOrRunOneM pid do
              questionLabeled "annuminas.warg"
              causedBy a
              progressChoice a 1
              targets characters exhaust
          else do
            mods <- getModifiers a
            let mayAttack = CannotAttack `notElem` mods && CannotAttackPlayer pid `notElem` mods
            if not mayAttack
              then removeProgressFromMainQuest a 2
              else chooseOrRunOneM pid do
                questionLabeled "annuminas.wargRider"
                causedBy a
                progressChoice a 2
                labeled "annuminas.immediateAttack" $ immediateAttack pid a.id
      pure e
    UseCardAbility _ (isSource a -> True) 2 windows -> do
      for_ [pid | Window When (EnemyAttacking eid pid) <- windows, eid == a.id] $ \pid ->
        chooseOrRunOneM pid do
          questionLabeled "annuminas.orc"
          causedBy a
          progressChoice a 1
          labeled "annuminas.defenseTwo" $ phaseModifier a a (DefenseModifier 2)
      pure e
    UseCardAbility _ (isSource a -> True) 3 windows -> do
      if toCardCode a == toCardCode Cards.savageWerewolf
        then removeProgressFromMainQuest a 3
        else for_ [pid | Window After (EnemyAttacking eid pid) <- windows, eid == a.id] $ \pid ->
          if a.placement == StagingArea
            then removeProgressFromMainQuest a 2
            else chooseOrRunOneM pid do
              questionLabeled "annuminas.host"
              causedBy a
              progressChoice a 2
              labeled "annuminas.returnToStaging" $ placeEnemy a.id StagingArea
      pure e
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      if
        | toCardCode a == toCardCode Cards.orcOfAngmar -> for_ defenders $ \target -> roundModifier (CardIdSource cid) target (DefenseModifier (-2))
        | toCardCode a == toCardCode Cards.angmarWargRider -> chooseAndExhaust a pid 1
        | toCardCode a == toCardCode Cards.savageWerewolf -> additionalAttack pid eid
        | toCardCode a == toCardCode Cards.vanguardOfCarnDum -> for_ defenders $ \target -> roundModifier (CardIdSource cid) target CannotReady
        | otherwise -> pure ()
      pure e
    _ -> AnnuminasInvader <$> runMessage msg a
