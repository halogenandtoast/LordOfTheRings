module LordOfTheRings.Treachery.Cards.ManOverboard (manOverboard) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (getStormcallerStage)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ManOverboard = ManOverboard TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

manOverboard :: TreacheryCard ManOverboard
manOverboard = treachery ManOverboard Cards.manOverboard

-- | If your heading is at the worst setting, this effect cannot be canceled.
instance HasModifiersFor ManOverboard where
  getModifiersFor (ManOverboard a) = whenM atWorstHeading $ modifySelf a [WhenRevealedCannotBeCanceled]

-- | When Revealed: Each player contributing at least 8 willpower to the quest must discard a
-- questing character.
-- Shadow: Attacking enemy gets +X attack, where X is the Stormcaller's current quest stage.
instance RunMessage ManOverboard where
  runMessage msg t@(ManOverboard attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        questing <- selectCharacters (HeroControlledBy (PlayerWithId p) <> CommittedHero) (AllyControlledBy (PlayerWithId p) <> CommittedAlly)
        willpower <- sum <$> traverse getWillpower questing
        when (willpower >= 8) do
          chooseOrRunOneM p do
            questionLabeled "manOverboard.discard"
            causedBy attrs
            targets questing (discard attrs)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      stage <- getStormcallerStage
      when (stage > 0) $ enemyGetsAttack attrs eid stage
      pure t
    _ -> ManOverboard <$> runMessage msg attrs
