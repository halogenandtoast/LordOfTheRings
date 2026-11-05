module LordOfTheRings.Quest.Cards.TheRingwraithsAttack (theRingwraithsAttack) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Nazgul))

newtype TheRingwraithsAttack = TheRingwraithsAttack QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRingwraithsAttack :: QuestCard TheRingwraithsAttack
theRingwraithsAttack = quest TheRingwraithsAttack Cards.theRingwraithsAttack

{- | When Revealed (3A): Add Weathertop and The Witch-king to the staging area. Search the
encounter deck and discard pile and add each Nazgûl enemy found to the staging area.
Shuffle the encounter deck.

If there are no Nazgûl enemies in play, the players win the game: read as one leaves play,
however it does (The Witch-king, added here, keeps one in play until then).
-}
instance RunMessage TheRingwraithsAttack where
  runMessage msg (TheRingwraithsAttack attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      addSetAsideToStaging (cardIs Locations.weathertop)
      addSetAsideToStaging (cardIs Enemies.theWitchKing)
      (deck, pile) <- getsGame \g -> let s = toAttrs g.scenario in (s.encounterDeck, s.encounterDiscard)
      let nazgul = filter (cardMatches $ CardWithTrait Nazgul <> CardWithType EnemyType)
      for_ (nazgul deck) \c -> removeFromEncounterDeck c >> addToStaging c
      for_ (nazgul pile) addToStagingFromDiscard
      shuffleEncounterDeck
      TheRingwraithsAttack <$> runMessage msg attrs
    CheckWindows ws | attrs.flipped, notNull [() | Window After (EnemyLeftPlay _) <- ws] -> do
      whenM (selectNone $ EnemyWithTrait Nazgul) winGame
      TheRingwraithsAttack <$> runMessage msg attrs
    _ -> TheRingwraithsAttack <$> runMessage msg attrs
