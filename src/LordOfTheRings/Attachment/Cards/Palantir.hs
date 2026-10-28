module LordOfTheRings.Attachment.Cards.Palantir (palantir) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (..), cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Noble))

newtype Palantir = Palantir AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noble hero.
palantir :: AttachmentCard Palantir
palantir = attachment Palantir Cards.palantir & attachTo (AttachToHero $ HeroWithTrait Noble)

-- | Planning Action: Exhaust Palantir and attached hero to name a card type and look at
-- the top 3 cards of the encounter deck. For each of those cards that matches the named
-- type, draw a card. For each of those cards that does not match the named type, raise
-- your threat by 2.
instance HasAbilities Palantir where
  getAbilities (Palantir a) =
    [ restricted a 1 (DuringPhase PlanningPhase) $ ActionAbility (Costs [exhaustCost a, ExhaustCost host])
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage Palantir where
  runMessage msg s@(Palantir a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      -- the type is named before the cards are looked at
      cards <- take 3 . (.encounterDeck) . toAttrs <$> getsGame (.scenario)
      let named types = do
            let matching = length [c | c <- cards, cdCardType (toCardDef c) `elem` types]
            drawCards pid matching
            raiseThreat pid (2 * (length cards - matching))
      chooseOneM pid do
        questionLabeled "palantir.name"
        causedBy a
        labeled "palantir.enemy" $ named [EnemyType]
        labeled "palantir.location" $ named [LocationType]
        labeled "palantir.treachery" $ named [TreacheryType]
        labeled "palantir.objective" $ named [ObjectiveType, ObjectiveAllyType]
      pure s
    _ -> Palantir <$> runMessage msg a
