module LordOfTheRings.Location.Cards.DrownedGraves (drownedGraves, drownedGravesNightmare) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

-- | Uncharted, under a Lost Island side.
newtype DrownedGraves = DrownedGraves LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

drownedGraves :: LocationCard DrownedGraves
drownedGraves = location DrownedGraves Cards.drownedGraves

-- | The Fate of Númenor Nightmare Deck's reprint, the same text.
drownedGravesNightmare :: LocationCard DrownedGraves
drownedGravesNightmare = location DrownedGraves Cards.drownedGravesNightmare

-- | Forced: After Drowned Graves is flipped over while active, discard the top X cards of
-- the encounter deck. Each player adds an Undead enemy discarded by this effect to the
-- staging area. X is 2 more than the number of players in the game.
instance HasAbilities DrownedGraves where
  getAbilities (DrownedGraves a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

{- | The players take the Undead enemies in turn order, each choosing one of those left:
the discarded cards come back for each player in turn, with that player as their target.
-}
instance RunMessage DrownedGraves where
  runMessage msg l@(DrownedGraves a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- getRemainingPlayers
      discardTopOfEncounterDeck a (length players + 2)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) target cards -> do
      players <- getRemainingPlayers
      let undead = filter (\c -> cdCardType (toCardDef c) == EnemyType && Undead `member` cdCardTraits (toCardDef c)) cards
          chooser = case target of
            PlayerTarget p -> Just p
            _ -> headMay players
          after p = drop 1 (dropWhile (/= p) players)
      for_ chooser \p -> unless (null undead) do
        chooseOneM p do
          questionLabeled "drownedGraves.choose"
          causedBy a
          targets undead \c -> do
            addToStagingFromDiscard c
            for_ (headMay $ after p) \next ->
              push $ DiscardedTopOfEncounterDeck (toSource a) (PlayerTarget next) (filter (/= c) undead)
      pure l
    _ -> DrownedGraves <$> runMessage msg a
