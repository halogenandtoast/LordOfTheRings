module LordOfTheRings.Quest.Cards.TheCursedShade (theCursedShade) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Undead))

newtype TheCursedShade = TheCursedShade QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCursedShade :: QuestCard TheCursedShade
theCursedShade = quest TheCursedShade Cards.theCursedShade

-- | Fram's Shade is immune to player card effects. While The Cursed Tomb is in the victory
-- display, Fram's Shade gains victory 6. This stage cannot be defeated unless Fram's Shade
-- is in the victory display. When the players defeat this stage, they win the game.
-- It is the last stage, so defeating it wins.
instance HasModifiersFor TheCursedShade where
  getModifiersFor (TheCursedShade a) = when a.flipped do
    victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
    let displayed def = any (cardMatches $ cardIs def) victory
    shades <- select (EnemyWithCardCode $ toCardCode Enemies.framsShade)
    for_ shades \s -> modified a s $ ImmuneToPlayerCardEffects : [GainVictory 6 | displayed Locations.theCursedTomb]
    unless (displayed Enemies.framsShade) $ modifySelf a [CannotBeDefeated]

-- | When Revealed: The first player adds The Cursed Tomb to the staging area. Each other
-- player searches the encounter deck and discard pile for a non-unique Undead enemy and
-- adds it to the staging area. Shuffle the encounter deck.
instance RunMessage TheCursedShade where
  runMessage msg q@(TheCursedShade attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheCursedShade <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.theCursedTomb)
      others <- drop 1 <$> getRemainingPlayers
      for_ others \p -> searchEncounterDeckAndDiscard p attrs (CardWithType EnemyType <> CardWithTrait Undead <> NotCard UniqueCard)
      shuffleEncounterDeck
      pure q
    _ -> TheCursedShade <$> runMessage msg attrs
