module LordOfTheRings.Treachery.Cards.HellHawk (hellHawk) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Mount, Nazgul))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HellHawk = HellHawk TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hellHawk :: TreacheryCard HellHawk
hellHawk = treachery HellHawk Cards.hellHawk

-- | Attached, it counts as a Mount attachment with the text: "Limit 1 per enemy. Attached
-- enemy gets +2 threat, +2 attack, +2 defense and is immune to player card effects."
instance HasModifiersFor HellHawk where
  getModifiersFor (HellHawk a) = withHost a \host -> do
    modifySelf a [GainTrait Mount]
    modified a host [ThreatModifier 2, AttackModifier 2, DefenseModifier 2, ImmuneToPlayerCardEffects]

nazgul :: CardMatcher
nazgul = CardWithType EnemyType <> CardWithTrait Nazgul

{- | When Revealed: Either search the encounter deck, discard pile, and victory display for a
Nazgûl enemy and add it to the staging area, or attach Hell-hawk to a Nazgûl enemy in play.
Each is offered while it can be done: a Nazgûl to find, or one in play without a Hell-hawk
(limit 1 per enemy).
-}
instance RunMessage HellHawk where
  runMessage msg t@(HellHawk attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      searchable <- getsGame \g ->
        let s = toAttrs g.scenario in any (cardMatches nazgul) (s.encounterDeck <> s.encounterDiscard) || any (cardMatches nazgul) s.victoryDisplay
      ridden <-
        getsGame
          $ (\ts -> [eid | x <- ts, toCardCode x == toCardCode Cards.hellHawk, AttachedTo (EnemyTarget eid) <- [x.placement]])
          . toList
          . (.entities.treacheries)
      mounts <- filter (`notElem` ridden) <$> select (EnemyWithTrait Nazgul)
      chooseOrRunOneM you do
        questionLabeled "hellHawk.choose"
        causedBy attrs
        when searchable $ labeled "hellHawk.search" $ searchEncounterCardsAndVictoryDisplayToStaging you attrs nazgul
        targets mounts \e -> attachTreachery attrs.treacheryId e
      pure t
    _ -> HellHawk <$> runMessage msg attrs
