module LordOfTheRings.Hero.Cards.Hama (hama) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype Hama = Hama HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hama :: HeroCard Hama
hama = hero Hama Cards.hama

tacticsEvent :: CardMatcher
tacticsEvent = CardWithType EventType <> CardWithSphere Tactics

-- | Response: After Háma is declared as an attacker, return a tactics event from your
-- discard pile to your hand. Then, choose and discard 1 card from your hand. (Limit 3
-- times per game for the group.) (FAQ 1.9 errata added the limit.)
instance HasAbilities Hama where
  getAbilities (Hama a) = errataOr a.owner (limited (GroupLimit PerGame 3) recur) recur
   where
    recur =
      restricted a 1 (CardInDiscard a.owner tacticsEvent)
        $ response (AttackerDeclaredWindow After (Just $ toTarget a))

instance RunMessage Hama where
  runMessage msg h@(Hama attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      events <- filter (cardMatches tacticsEvent) . (.discard) <$> getPlayer pid
      chooseOneM pid do
        questionLabeled "hama.return"
        causedBy attrs
        targets events \c -> returnFromDiscardToHand pid c.id
      chooseAndDiscardFromHand pid attrs 1
      pure h
    _ -> Hama <$> runMessage msg attrs
