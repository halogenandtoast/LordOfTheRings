module LordOfTheRings.Treachery.Cards.PrancingPonyTreacheries
  (takenBySurprise, hiredMuscle, smokeAndMirrors, risingBodyCount, deadlyAccusations) where

import LordOfTheRings.Card (EncounterCard (..), cdThreat, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource)
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers (activeInvestigateValue, lookAtBottom)
import LordOfTheRings.Trait (Trait (Condition, Brigand))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

-- Taken by Surprise's out-of-play Forced effect is resolved by the investigation
-- deck whenever it is looked at, not only by the Investigate keyword.
newtype TakenBySurprise = TakenBySurprise TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

takenBySurprise :: TreacheryCard TakenBySurprise
takenBySurprise = treachery TakenBySurprise Cards.takenBySurprise

newtype HiredMuscle = HiredMuscle TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

hiredMuscle :: TreacheryCard HiredMuscle
hiredMuscle = treachery HiredMuscle Cards.hiredMuscle

instance HasModifiersFor HiredMuscle where
  getModifiersFor (HiredMuscle a) = withHost a $ \host -> do
    modifySelf a [GainTrait Condition]
    modified a host [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]

instance HasAbilities HiredMuscle where
  getAbilities (HiredMuscle a) = case a.placement of
    AttachedTo host@(EnemyTarget eid) ->
      [restricted a 1 (TargetLacksModifier host Blank) $ forced $ ThisEnemyDefeatedWindow When eid]
    _ -> []

instance RunMessage HiredMuscle where
  runMessage msg t@(HiredMuscle a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      lead <- getLead
      attached <- getsGame $ \g -> [spell.placement | spell <- toList g.entities.treacheries, toCardCode spell == toCardCode Cards.hiredMuscle]
      enemies <- filterM (eligible attached) =<< select (EnemyWithTrait Brigand)
      chooseOrRunOneM lead do
        questionLabeled "prancingPony.hiredMuscle"
        causedBy a
        targets enemies $ \eid -> do
          attachTreachery a.id (EnemyTarget eid)
          placeEnemy eid StagingArea
      pure t
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      lookAtBottom lead a
      pure t
    _ -> HiredMuscle <$> runMessage msg a
   where
    eligible attached eid = do
      mods <- getModifiers eid
      immune <- immuneToSource (toSource a) eid
      pure $ not immune && AttachedTo (EnemyTarget eid) `notElem` attached
        && CannotHaveAttachments `notElem` mods && CannotHaveTreacheryAttachments `notElem` mods
        && all (== Condition) [trait | CannotHaveAttachmentsWithoutTrait trait <- mods]

newtype PrancingPonyTreacheries = PrancingPonyTreacheries TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

smokeAndMirrors, risingBodyCount, deadlyAccusations :: TreacheryCard PrancingPonyTreacheries
smokeAndMirrors = treachery PrancingPonyTreacheries Cards.smokeAndMirrors
risingBodyCount = treachery PrancingPonyTreacheries Cards.risingBodyCount
deadlyAccusations = treachery PrancingPonyTreacheries Cards.deadlyAccusations

instance RunMessage PrancingPonyTreacheries where
  runMessage msg t@(PrancingPonyTreacheries a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      if
        | toCardCode a == toCardCode Cards.smokeAndMirrors -> discardTopOfEncounterDeck a . (+ 1) =<< activeInvestigateValue
        | toCardCode a == toCardCode Cards.deadlyAccusations -> do
            amount <- (+ 1) <$> activeInvestigateValue
            eachPlayer (`encounterRaisesThreat` amount)
        | otherwise -> do
            lead <- getLead
            chooseOneM lead do
              questionLabeled "prancingPony.risingBodyCount"
              causedBy a
              labeled "prancingPony.raiseEachThreat2" $ eachPlayer (`encounterRaisesThreat` 2)
              labeled "prancingPony.shuffleBodyCount" do
                removeFromGame a
                push $ ShuffleIntoInvestigationDeck $ MkEncounterCard a.cardId (toCardCode a)
      pure t
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      phaseModifier a GameTarget $ StagingThreatBonus $ sum $ map (fromMaybe 0 . cdThreat . toCardDef) cards
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      if
        | toCardCode a == toCardCode Cards.smokeAndMirrors -> discardNonObjectiveAttachmentBy a (CardIdSource cid) pid
        | toCardCode a == toCardCode Cards.deadlyAccusations -> enemyGetsAttack a eid =<< activeInvestigateValue
        | otherwise -> ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      chooseOneM pid do
        questionLabeled "prancingPony.risingBodyCount"
        causedBy a
        labeled "prancingPony.raiseThreat2" $ encounterRaisesThreat pid 2
        labeled "prancingPony.shuffleBodyCount" do
          push $ RemoveShadowCard eid cid
          push $ ShuffleIntoInvestigationDeck $ MkEncounterCard cid (toCardCode a)
      pure t
    _ -> PrancingPonyTreacheries <$> runMessage msg a
