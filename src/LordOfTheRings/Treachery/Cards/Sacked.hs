module LordOfTheRings.Treachery.Cards.Sacked (sacked) where

import LordOfTheRings.Card (CardDef (..), toCardCode, toCardDef)
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Trait (Trait (Troll))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Sacked = Sacked TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sacked :: TreacheryCard Sacked
sacked = treachery Sacked Cards.sacked

-- | Counts as a condition attachment with the text: "Attached hero cannot attack, defend,
-- commit to a quest, trigger its effect, or collect resources."
--
-- While it is being revealed, unattached, its "when revealed" effect cannot be canceled.
instance HasModifiersFor Sacked where
  getModifiersFor (Sacked a) = do
    when (a.placement == Unplaced) $ modified a GameTarget [CannotCancelEncounterEffects]
    withHost a \host ->
      modified a host [CannotAttackOrDefend, CannotCommitToQuest, CannotTriggerAbilities, CannotCollectResources]

-- | When Revealed: Attach to a hero with no "Sacked!" cards attached controlled by the first
-- player. (Cannot be canceled.)
-- Shadow: If attacking enemy is a Troll, resolve this card's "when revealed" effect.
instance RunMessage Sacked where
  runMessage msg t@(Sacked attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      chooseHero \h -> attachTreachery attrs.treacheryId h
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      troll <- member Troll . cdCardTraits . toCardDef <$> getEnemy eid
      when troll $ chooseHero $ attachShadowTreachery eid cid
      pure t
    _ -> Sacked <$> runMessage msg attrs
   where
    chooseHero f = do
      lead <- getLead
      heroes <- select (HeroControlledBy (PlayerWithId lead))
      taken <- hostsOfTreachery (toCardCode Cards.sacked)
      chooseOrRunOneM lead do
        questionLabeled "sacked.attach"
        causedBy attrs
        targets (filter (\h -> not $ any (isTarget h) taken) heroes) f
