module LordOfTheRings.Treachery.Cards.Grappled (grappled) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Grappled = Grappled TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

grappled :: TreacheryCard Grappled
grappled = treachery Grappled Cards.grappled

-- | Counts as a Condition attachment with the text: "Attached objective cannot ready.
-- Action: Exhaust 3 characters you control to discard Grappled!"
instance HasModifiersFor Grappled where
  getModifiersFor (Grappled a) = withHost a \host -> modified a host [CannotReady]

instance HasAbilities Grappled where
  getAbilities (Grappled a) = case a.placement of
    AttachedTo _ ->
      [ anyPlayer
          $ restricted a 1 (CharactersAtLeast 3 (HeroControlledBy ActivePlayer <> ReadyHero) (AllyControlledBy ActivePlayer <> ReadyAlly))
          $ ActionAbility
          $ Costs (replicate 3 $ ExhaustCharacterCost (HeroControlledBy ActivePlayer) (AllyControlledBy ActivePlayer))
      ]
    _ -> []

-- | When Revealed: Attach to a Ship objective.
-- Shadow: If defending character is a Ship, attach Grappled! to it.
instance RunMessage Grappled where
  runMessage msg t@(Grappled attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      ships <- select shipObjective
      chooseOrRunOneM lead do
        questionLabeled "grappled.attach"
        causedBy attrs
        targets ships \s -> attachTreachery attrs.treacheryId s
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      ships <- filterM (\aid -> selectAny (shipObjective <> AllyWithId aid)) [aid | AllyTarget aid <- defenders]
      chooseOrRunOneM pid do
        questionLabeled "grappled.attach"
        causedBy (CardIdTarget cid)
        targets ships $ attachShadowTreachery eid cid
      pure t
    -- the characters are exhausted as the cost is paid
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discard attrs
      pure t
    _ -> Grappled <$> runMessage msg attrs
