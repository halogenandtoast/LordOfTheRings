module LordOfTheRings.Attachment.Cards.Thorongil (thorongil) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (HeroType), cdCardType, cdName, cdSphere, genPlayerCard, toCardCode, toCardDef)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Hero.CardDefs (allHeroCards)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Sphere (Sphere (..))

newtype Thorongil = Thorongil AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a non-Fellowship, non-Baggins hero you control.
thorongil :: AttachmentCard Thorongil
thorongil = attachment Thorongil Cards.thorongil & fmap (overAttrs \a -> a {attachmentAttachTo = AttachToHero $ eligible a})

eligible :: AttachmentAttrs -> HeroMatcher
eligible a = HeroControlledBy (PlayerWithId a.controller) <> not_ (HeroWithSphere Fellowship) <> not_ (HeroWithSphere Baggins)

-- | The other non-Fellowship, non-Baggins versions of the hero: your collection is every
-- hero the game has, as Helm of Secrecy reads it.
versionsOf :: CardCode -> [CardCode]
versionsOf code =
  [ c
  | c <- Map.keys allHeroCards
  , c /= code
  , cdCardType (toCardDef c) == HeroType
  , cdSphere (toCardDef c) `notElem` [Just Fellowship, Just Baggins]
  , title c == title code
  ]
 where
  title c = let Name t _ = cdName (toCardDef c) in t

-- | The names of the heroes with another version.
withOtherVersions :: [Name]
withOtherVersions = nub [cdName (toCardDef c) | c <- Map.keys allHeroCards, notNull (versionsOf c)]

-- | While Thorongil is in play, attached hero gains the game text and resource icon of the
-- attached hero card. If Thorongil leaves play, the hero card attached to it goes with it,
-- removed from the game.
instance HasModifiersFor Thorongil where
  getModifiersFor (Thorongil a) = case (a.placement, a.underneath) of
    (AttachedTo host@(HeroTarget _), PlayerCard pc : _) ->
      modified a host $ GainGameTextOf (toCardCode pc) : [GainResourceIcon s | Just s <- [cdSphere (toCardDef pc)]]
    _ -> pure ()

-- | Response: After you attach Thorongil to a hero, search your collection for a different
-- non-Fellowship, non-Baggins version of that hero and attach it to Thorongil.
instance HasAbilities Thorongil where
  getAbilities (Thorongil a) =
    [ restricted a 1 (exists $ HeroWithId hid <> HeroMatchAny (map HeroWithTitle withOtherVersions))
        $ response
        $ AttachedToHeroWindow After (toTarget a)
    | AttachedTo (HeroTarget hid) <- [a.placement]
    , null a.underneath
    ]

instance RunMessage Thorongil where
  runMessage msg s@(Thorongil a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [a.placement]] \hid -> do
        host <- getHero hid
        cards <- map PlayerCard <$> traverse (\c -> genPlayerCard (toCardDef c) (Just pid)) (versionsOf $ toCardCode host)
        chooseOrRunOneM pid do
          questionLabeled "thorongil.chooseVersion"
          causedBy a
          showing cards
          targets cards \c -> placeUnderneath a [c]
      pure s
    -- Helm of Secrecy swapping its hero out: Thorongil is connected to that hero, so it is
    -- no eligible attachment to move to the hero swapped in, whatever version that is, and
    -- is discarded (Caleb Grace, ALeP FAQ 3.374). Only the hero leaving can hold it.
    SwapHero hid _ | a.placement == AttachedTo (HeroTarget hid) ->
      pure $ Thorongil a {attachmentAttachTo = AttachToHero (HeroWithId hid)}
    _ -> Thorongil <$> runMessage msg a
