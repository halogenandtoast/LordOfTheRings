module LordOfTheRings.Treachery.Cards.EasterlingHorse (easterlingHorse) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Easterling, Mount))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EasterlingHorse = EasterlingHorse TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingHorse :: TreacheryCard EasterlingHorse
easterlingHorse = treachery EasterlingHorse Cards.easterlingHorse

-- | While attached to an enemy, counts as a Mount attachment with the text: "Attached enemy
-- gets +2 Threat. Forced: At the end of the round, return attached enemy to the staging
-- area."
instance HasModifiersFor EasterlingHorse where
  getModifiersFor (EasterlingHorse a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> do
      modifySelf a [GainTrait Mount]
      modified a host [ThreatModifier 2]
    _ -> pure ()

instance HasAbilities EasterlingHorse where
  getAbilities (EasterlingHorse a) = case a.placement of
    AttachedTo (EnemyTarget _) -> [mkAbility a 1 $ forced RoundEndsWindow]
    _ -> []

-- | When Revealed: Attach to the lowest engagement cost Easterling enemy without an
-- Easterling Horse and return it to the staging area. Otherwise, Easterling Horse gains
-- surge.
instance RunMessage EasterlingHorse where
  runMessage msg t@(EasterlingHorse attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      attached <-
        attachToEnemyWith attrs attrs.treacheryId attrs.treacheryCardCode (EnemyWithTrait Easterling) Lowest getEngagementCost returnToStaging
      unless attached revealEncounterCard
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      case attrs.placement of
        AttachedTo (EnemyTarget eid) -> returnToStaging eid
        _ -> pure ()
      pure t
    _ -> EasterlingHorse <$> runMessage msg attrs
   where
    returnToStaging eid = do
      placement <- (.placement) <$> getEnemy eid
      unless (placement == StagingArea) $ placeEnemy eid StagingArea
