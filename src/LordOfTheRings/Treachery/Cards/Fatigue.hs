module LordOfTheRings.Treachery.Cards.Fatigue (fatigue) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Fatigue = Fatigue TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fatigue :: TreacheryCard Fatigue
fatigue = treachery Fatigue Cards.fatigue

-- | When Revealed: Each player must exhaust 1 character he controls, if able. Then, if any
-- player controls no unexhausted characters, Fatigue gains surge.
-- Shadow: The defending player must exhaust 1 character he controls, if able.
instance RunMessage Fatigue where
  runMessage msg t@(Fatigue attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseAndExhaust attrs p 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      spent <- anyM (fmap null . readyCharactersOf) players
      when spent revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure t
    _ -> Fatigue <$> runMessage msg attrs
   where
    readyCharactersOf p = selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
