module LordOfTheRings.Attachment.Cards.TheArkenstone (theArkenstone) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype TheArkenstone = TheArkenstone AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy or location), as its card def's keyword. Attach to a hero.
theArkenstone :: AttachmentCard TheArkenstone
theArkenstone = attachment TheArkenstone Cards.theArkenstone

-- | Each unique character you control gets +1 willpower: you, its controller, who controls
-- the hero it is attached to.
instance HasModifiersFor TheArkenstone where
  getModifiersFor (TheArkenstone a) = when (isAttached a.placement) do
    let you = PlayerWithId a.controller
    uniques <- selectCharacters (HeroControlledBy you) (AllyControlledBy you <> UniqueAlly)
    for_ uniques \c -> modified a c [WillpowerModifier 1]

-- | Action: Reduce your threat by 2 to attach The Arkenstone to a hero controlled by another
-- player and raise that player's threat by 2. (Limit once per round.) The limit is the
-- card's, wherever it goes.
instance HasAbilities TheArkenstone where
  getAbilities (TheArkenstone a) =
    [ limited (GroupLimit PerRound 1)
        $ restricted
          a
          1
          (exists (HeroControlledBy $ NotPlayer you) <> TargetLacksModifier (PlayerTarget pid) CannotReduceThreat)
        $ ActionAbility Free
    | isAttached a.placement
    , let pid = a.controller
          you = PlayerWithId pid
    ]

isAttached :: Placement -> Bool
isAttached = \case
  AttachedTo _ -> True
  _ -> False

instance RunMessage TheArkenstone where
  runMessage msg s@(TheArkenstone a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      lowerThreatBy a pid 2
      heroes <- select (HeroControlledBy $ NotPlayer $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "theArkenstone.hero"
        causedBy a
        targets heroes \hid -> do
          moveAttachment a.id hid
          other <- (.owner) <$> getHero hid
          raiseThreat other 2
      pure s
    _ -> TheArkenstone <$> runMessage msg a
