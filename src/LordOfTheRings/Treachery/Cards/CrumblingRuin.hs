module LordOfTheRings.Treachery.Cards.CrumblingRuin (crumblingRuin) where

import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Helpers.Character (getHitPoints)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CrumblingRuin = CrumblingRuin TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crumblingRuin :: TreacheryCard CrumblingRuin
crumblingRuin = treachery CrumblingRuin Cards.crumblingRuin

-- | When Revealed: Each player must exhaust a character and discard the top card of his
-- deck, if able. If the printed cost of the discarded card is equal to or higher than the
-- remaining hit points of the exhausted character, discard the exhausted character.
instance RunMessage CrumblingRuin where
  runMessage msg t@(CrumblingRuin attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        able <- selectCharacters (HeroControlledBy (PlayerWithId p) <> ReadyHero) (AllyControlledBy (PlayerWithId p) <> ReadyAlly)
        top <- headMay . (.deck) <$> getPlayer p
        -- a card with no printed number (X) costs 0
        let cost = case cdCost . toCardDef <$> top of
              Just (Just (StaticCost n)) -> Just n
              Just _ -> Just 0
              Nothing -> Nothing
        if null able
          then discardTopOfDeck p 1
          else chooseOrRunOneM p do
            questionLabeled "crumblingRuin.exhaust"
            causedBy attrs
            targets able \c -> do
              remaining <- (-) <$> getHitPoints c <*> getDamage c
              exhaust c
              discardTopOfDeck p 1
              for_ cost \n -> when (n >= remaining) $ discard c
      pure t
    _ -> CrumblingRuin <$> runMessage msg attrs
