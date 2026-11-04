module LordOfTheRings.Objective.Cards.UrdugsHorn (urdugsHorn) where

import LordOfTheRings.Card (cdThreat, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype UrdugsHorn = UrdugsHorn ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

urdugsHorn :: ObjectiveCard UrdugsHorn
urdugsHorn = objective UrdugsHorn Cards.urdugsHorn

-- | Urdug, objective-ally side faceup, wherever he is: a captive too.
getUrdug :: HasGame m => m (Maybe AllyId)
getUrdug = getsGame (fmap toId . find ((== toCardCode Cards.urdug) . toCardCode) . toList . (.entities.allies))

-- | While Urdug's Horn is free of encounters, attach it to Urdug: the first player claims
-- it for him.
instance HasModifiersFor UrdugsHorn where
  getModifiersFor (UrdugsHorn a) = when (a.placement == StagingArea) do
    urdug <- getUrdug
    when (isJust urdug) $ modifySelf a [FirstPlayerClaimsWhenFree]

-- | Forced: When Urdug attacks, discard the top card of the encounter deck and add its
-- threat to his attack until the end of the phase. He keeps the horn as he turns into an
-- enemy (Vision of the Palantir's Roam Across Rhovanion review): his attacks as an ally
-- and as an enemy both count.
instance HasAbilities UrdugsHorn where
  getAbilities (UrdugsHorn a) = case a.placement of
    AttachedTo host@(AllyTarget _) -> [mkAbility a 1 $ forced $ AttackerDeclaredWindow After (Just host)]
    AttachedTo (EnemyTarget eid) -> [mkAbility a 1 $ forced $ EnemyAttacksWindow When eid]
    _ -> []

instance RunMessage UrdugsHorn where
  runMessage msg o@(UrdugsHorn attrs) = case msg of
    ClaimFreeObjective oid | oid == attrs.id -> do
      traverse_ (attachObjective attrs.id) =<< getUrdug
      pure o
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      let threat = sum [n | c <- cards, Just n <- [cdThreat (toCardDef c)]]
      for_ [host | AttachedTo host <- [attrs.placement]] \host ->
        when (threat > 0) $ phaseModifier attrs host (AttackModifier threat)
      pure o
    _ -> UrdugsHorn <$> runMessage msg attrs
