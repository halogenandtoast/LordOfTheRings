module LordOfTheRings.Treachery.Cards.RotAndVapors (rotAndVapors) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (immuneToSource)
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (placeMireOnActiveLocation)

newtype RotAndVapors = RotAndVapors TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rotAndVapors :: TreacheryCard RotAndVapors
rotAndVapors = treachery RotAndVapors Cards.rotAndVapors

instance RunMessage RotAndVapors where
  runMessage msg t@(RotAndVapors a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      eachPlayer \pid -> do
        questing <- selectCharacters (CommittedHero <> HeroControlledBy (PlayerWithId pid)) (CommittedAlly <> AllyControlledBy (PlayerWithId pid))
        characters <- filterM (fmap not . immuneToSource (toSource a)) =<< charactersControlledBy pid
        chooseOneM pid do
          questionLabeled "rotAndVapors.choose"
          causedBy a
          labeled "rotAndVapors.threat" $ encounterRaisesThreat pid (length questing)
          labeled "rotAndVapors.damage" $ for_ characters (\character -> dealDamage a character 1)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack a eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      placeMireOnActiveLocation a
      pure t
    _ -> RotAndVapors <$> runMessage msg a
